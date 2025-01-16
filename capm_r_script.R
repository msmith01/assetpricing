rm(list=ls())
options(scipen=999)

# CAPM in R

library(tseries)
library(quantmod)
stock = c("LTPZ", "MSFT", "GOOG", "F", "PEP", "SPY", "GC=F")
# stock <- c("AAPL", "ABBV", "A",  "APD", "AA", "CF", "NVDA", "HOG", "WMT", "AMZN"
#              ,"MSFT", "INTC", "ADBE", "AMG", "AKAM", "ALB", "ALK", "V", "PG", "COST", "ADBE", "KO", "BAC", "HD", "VZ", "AMGN", "TXN"
#              , "UBER", "LOW", "NKE", "AMAT", "GS", "LMT", "PGR", "ADP", "ETN", "CDNS", "CME", "FCX", "APH"
# )

getSymbols(stock)

stocks = do.call(merge, lapply(stock, function(x){Cl(get(x))}))
#stocks = stocks[year(index(stocks)) >= 2018 & year(index(stocks)) <= 2024]

stock_returns = diff(log(stocks))

risk_free_rate = 0.06
trading_days = 252

# compute the risk premium
risk_premium = stock_returns - (risk_free_rate / trading_days)

risk_premium %>% 
  data.frame() %>% 
  pivot_longer(cols = -SPY.Close) %>% 
  ggplot(aes(x = SPY.Close, y = value)) +
  geom_point() +
  facet_wrap(~name) +
  geom_smooth(method = "lm", color = "red") + 
  theme_bw()


# Compute the CAPM model
colnames(risk_premium)
lm(risk_premium$PEP.Close ~ risk_premium$SPY) %>% 
  summary()



# compute the monthly stock returns
# monthly_stock_returns = diff(log(stocks), lag = 21)
daily_returns <- diff(log(stocks))

# Convert 'daily_returns' to a monthly format
monthly_returns_list <- lapply(daily_returns, function(column_data) {
  apply.monthly(column_data, sum)
})

# Convert the list of xts objects back into a single xts object/dataframe
monthly_returns_xts <- do.call(merge, monthly_returns_list)

lm(monthly_returns_xts$PEP.Close ~ monthly_returns_xts$SPY.Close) %>% 
  summary() # check with Yahoo finance betas - 0.88 in YF and here 0.88 also for MSFT


lm(monthly_returns_xts$F.Close ~ monthly_returns_xts$SPY.Close) %>% 
  summary()

lm(monthly_returns_xts$PEP.Close ~ monthly_returns_xts$SPY.Close) %>% 
  summary() # moves less than the SPY500 0.54 - slope is flatter than other stocks


################################################################################
################################################################################
################################################################################

# Fama and French 3 factor models

rm(list=ls())

# build the following portfolio

# + SPY (S&P500 fund) weighted 25%
# + EFA (a non-US equities fund) weighted 25%
# + IJS (a small-cap value fund) weighted 20%
# + EEM (an emerging-mkts fund) weighted 20%
# + AGG (a bond fund) weighted 10%

# download and compute portfolio monthly returns

library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(glue)

symbols = c("AAPL", "MSFT", "GOOG", "F", "PEP")
# symbols <- c("SPY","EFA", "IJS", "EEM","AGG")
#symbols = c("AG", "MUX", "SOS", "HUYA", "MGNI")

prices <- 
  getSymbols(symbols,
             src = 'yahoo', 
             from = "2012-12-31",
             to = "2023-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)
# w <- c(0.1, 0.1, 0.1, 0.5, 0.2)

asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

asset_returns_wide = asset_returns_long %>% 
  pivot_wider(names_from = asset, values_from = returns)

# do one SPY example
# 2012-12-31 = adj close = 116.5059
# 2013-01-31 = adj close = 122.4699

#log(122.46989) - log(116.50594) # rounding differences so not exact as the df

# construct the portfolio using the weights
portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")

# Now is our portfolio tilted more towards certain factors

# Download FF factors data - thankfully they are already calculated for us - so we don0t need to sort and calculate them ourselves
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html

temp <- tempfile()

base <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
factor <- "Global_3_Factors"
format <- "_CSV.zip"
full_url <- glue(base, factor, format, sep ="")

download.file(full_url, temp, quiet = TRUE)

Global_3_Factors <- read_csv(unz(temp, "Global_3_Factors.csv"), skip = 6) %>% 
  rename(date = c(1)) %>% 
  mutate_at(vars(-date), as.numeric) %>% 
  mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>% 
  mutate(date = rollback(date + months(1)))
  #mutate(date = lubridate::rollback(date)) # NOTE -- FF uses the first of the month whereas portfolios are using the last of the month 
# - so we use lubridates rollback function - to roll the date back to the last date of the previous month

# Note useful
# Global_3_Factors %>% 
#   mutate(date = lubridate::rollback(date + months(1)))


ff_portfolio_returns = portfolio_returns_tq_rebalanced_monthly %>% 
  left_join(Global_3_Factors, by = "date") %>% 
  mutate(MKT_RF = `Mkt-RF`/100,
         SMB = SMB/100,
         HML = HML/100,
         RF = RF/100,
         R_excess = round(returns - RF, 4))



ff_portfolio_returns
# Now we have our weighted portfolio returns and the FF 3 factors

ff_dplyr_byhand = lm(R_excess ~ MKT_RF + SMB + HML, data = ff_portfolio_returns) %>% 
  tidy(conf.int = T, conf.level = .95) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  select(-statistic)

### Show examples on how the SMB changes when we use large cap stocks and small cap stocks in our portfolio
# more tilted to larger book value stocks when we invest in symbols = c("AAPL", "MSFT", "GOOG", "F", "PEP")
# hence the negative value for SMB - 0.744 
# HML is also negative - and stat sig
# Market beta is almost 1 (0.95) so portfolio moves with the market or just less
# when we invest in c("AG", "MUX", "SOS", "HUYA", "MGNI") we have a 1.54 positive value  - not stat sig
lm(R_excess ~ MKT_RF + SMB + HML, data = ff_portfolio_returns) %>% 
  summary()
# Interpret the lm summary

  
ff_dplyr_byhand %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>%
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, shape = term, color = term)) + 
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  labs(title = "FF 3-Factor Coefficients for Our Portfolio",
       subtitle = "nothing in this post is investment advice",
       x = "",
       y = "coefficient",
       caption = "data source: Fama French website and yahoo! Finance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption  = element_text(hjust = 0))

  