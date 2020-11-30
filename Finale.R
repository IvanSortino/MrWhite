rm(list = ls())
graphics.off()

library(tidyverse)
library(tidyquant)

myfunc <- function(symbol, begin_date, end_date, begin_date_sample, end_date_sample, nsim, t) {
stock <- tq_get(symbol, get  = "stock.prices", from = begin_date, to = end_date)
close_price <- stock %>% select(date, close, symbol) %>% 
  pivot_wider(names_from = symbol, values_from = close)

close_price_sample <- close_price %>% filter(date >= begin_date_sample & date <= end_date_sample)

rets <- close_price_sample %>% select(symbol) %>% pull() %>% RETURN() %>% na.exclude()
mu <- mean(rets) ### here I think that there is an issue about time scaling
sigma <- sd(rets) * sqrt(250)
s0 <- tail(close_price_sample, n = 1) %>% pull()

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

close_price_forecast_act. <- close_price %>% filter(date > end_date_sample)
close_price_forecast_act. <- select(close_price_forecast_act.[1:t, ], symbol)
close_price_forecast_act. <- as.matrix(close_price_forecast_act.)

diff_mat_MAPE <- matrix(nrow = nrow(gbm), ncol = ncol(gbm))
for (i in 1:ncol(gbm)) {
  diff_mat_MAPE[, i] <- abs((close_price_forecast_act. - gbm[, i]) / close_price_forecast_act.)
}

MAPE <- colSums(diff_mat_MAPE) / nrow(gbm)

res <- tibble(mod = c('FALSE', 'TRUE'),
              '[0, 0.1]' = table(MAPE <= 0.10),
              '(0.1, 0.2]' = table(MAPE > 0.10 & MAPE <= 0.2),
              '(0.2, 0.5]'= table(MAPE > 0.2 & MAPE <= 0.5),
              '(0.5, 1]' = table(MAPE > 0.5)) ### Last table gives an incorrect result since TRUE = 0

# best_MAPE_position <- which(MAPE <= 0.10) ; best_MAPE <- gbm[, best_MAPE_position]
# df_forPlot <- as.data.frame(best_MAPE) %>%
#   mutate(ix = 1:nrow(best_MAPE)) %>% pivot_longer(-ix, names_to = 'best_sim', values_to = 'price') 
# rep_actual <- c()
# for (i in 1:nrow(close_price_july)) {
#  rep_actual <- rep(ncol(close_price_july[i, ], best_MAPE))
#}
# df_forPlot %>%
#  ggplot(aes(x = ix, y = price, color = best_sim)) +
#  geom_line() 

return(res)

}

myfunc('AMZN', '2019-01-01', '2019-12-31', '2019-01-01', '2019-03-01', 100, 10)
# High volatility in the following
myfunc('AMZN', '2019-01-01', '2020-08-31', '2020-01-01', '2020-03-01', 100, 10) ### Problem because first TRUE = 0
myfunc('AMZN', '2019-01-01', '2020-08-31', '2020-01-01', '2020-03-01', 1000, 10)

myfunc('AMZN', '2019-01-01', '2020-08-31', '2020-01-01', '2020-03-01', 100, 10)
