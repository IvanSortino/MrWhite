rm(list = ls())
graphics.off()

library(tidyverse)
library(tidyquant)
## myfunc('AMZN', '2019-01-02', '2019-12-31', '2019-02-01', '2019-03-01', 1000, 10)
myfunc <- function(symbol, begin_date, end_date, begin_date_sample, end_date_sample, nsim, t) {
stock <- tq_get(symbol, get  = "stock.prices", from = begin_date, to = end_date)
close_price <- stock %>% select(date, close, symbol) %>% 
  pivot_wider(names_from = symbol, values_from = close)

close_price_sample <- close_price %>% filter(date > begin_date_sample & date < end_date_sample)

rets <- close_price_sample %>% select(symbol) %>% pull() %>%  RETURN()  #perche select(symbol)?
rets <- rets[-1]
mu <- mean(rets) 
sigma <- sd(rets)
s0 <- close_price %>% filter(date == end_date_sample) %>% pull() #perche non begin date sample?

gbm_fun <- function(nsim, t, mu, sigma, S0, dt = 1) {
  gbm <- matrix(ncol = nsim, nrow = t)
  for (simu in 1:nsim) {
    gbm[1, simu] <- S0
    for (day in 2:t) {
      epsilon <- rnorm(1)
      gbm[day, simu] <- gbm[(day-1), simu] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    }
  }
  return(gbm)
}

gbm <- gbm_fun(nsim, t, mu, sigma, s0, 1)

close_price_forecast <- close_price %>% filter(date > end_date_sample)
close_price_forecast <- select(close_price_forecast[1:t, ], symbol)
close_price_forecast <- as.matrix(close_price_forecast)

diff_mat_MAPE <- matrix(nrow = nrow(gbm), ncol = ncol(gbm))
for (i in 1:ncol(gbm)) {
  diff_mat_MAPE[, i] <- abs((close_price_forecast - gbm[, i]) / close_price_forecast)
}

MAPE <- colSums(diff_mat_MAPE) / nrow(gbm)

t10 <- table(MAPE <= 0.10)
t20 <- table(MAPE > 0.10 & MAPE <= 0.2)
t50 <- table(MAPE > 0.2 & MAPE <= 0.5)
t100 <- table(MAPE > 0.5)
table

best_MAPE_position <- which(MAPE <= 0.10)
best_MAPE <- gbm[, best_MAPE_position]
return(t10)

}

