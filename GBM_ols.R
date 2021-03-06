rm(list = ls())
graphics.off()

library(tidyverse)
library(tidyquant)

stock <- tq_get('AAPL', get  = "stock.prices", from = "2019-01-01", to   = "2019-09-01")
close_price <- stock %>% select(date, close, symbol) %>% 
  pivot_wider(names_from = symbol, values_from = close)

close_plot <- ggplot(close_price, aes(date, AAPL)) +
  geom_line() +
  labs(x = 'Date')
close_plot

close_price_june <- close_price %>% filter(date > "2019-05-31" & date < "2019-07-01")
close_plot_june <- ggplot(close_price_june, aes(date, AAPL)) +
  geom_line() +
  labs(x = 'Date')
close_plot_june

rets <- close_price_june %>% select(AAPL) %>% pull() %>%  RETURN() 
rets <- rets[-1]
mu <- mean(rets) 
sigma <- sd(rets)
s0 <- close_price %>% filter(date == "2019-06-28") %>% pull()

# In the following code we have created a matrix in which rows represent each observation at time t
# while in columns there are the simulations 

gbm_fun <- function(nsim, t, mu, sigma, S0, dt) {
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

gbm <- gbm_fun(50, 30, mu, sigma, s0, 1)
set.seed(123)
gbm_df <- as.data.frame(gbm) %>%
  mutate(ix = 1:nrow(gbm)) %>%
  pivot_longer(-ix, names_to = 'sim', values_to = 'price')

gbm_df %>%
  ggplot(aes(x=ix, y=price, color=sim)) +
  geom_line() +
  theme(legend.position = 'none')

# Here we want to find the better and worst model for our stock series. To achieve this aim 
# we have minimized the sum of the square error 

close_price_july <- close_price %>% filter(date >= "2019-06-28")
close_price_july <- select(close_price_july[1:30, ], 'AAPL')
close_price_july <- as.matrix(close_price_july)

diff_mat <- matrix(nrow = nrow(gbm), ncol = ncol(gbm))
for (i in 1:ncol(gbm)) {
  diff_mat[, i] <- close_price_july - gbm[, i]
}

ols <- colSums(diff_mat ^ 2)
best_ols <- min(ols)
best_ols_position <- which(ols == best_ols)
best_forecast <- gbm[, best_ols_position]

worst_ols <- max(ols)
worst_ols_position <- which(ols == worst_ols)
worst_forecast <- gbm[, worst_ols_position]

df_compare <- data.frame(actual = close_price_july,
                         b_forecast = best_forecast,
                         w_forecast = worst_forecast,
                         date = 1:30)

lower5 <- exp(log(s0)+(mu-sigma*sigma/2)*1:30+qnorm(0.025)*sigma*sqrt(1:30))
upper5 <- exp(log(s0)+(mu-sigma*sigma/2)*1:30+qnorm(0.975)*sigma*sqrt(1:30))
df_confint5 <- data.frame(ix = 1:30,
                         l_95 = lower5,
                         u_95 = upper5)

lower1 <- exp(log(s0)+(mu-sigma*sigma/2)*1:30+qnorm(0.005)*sigma*sqrt(1:30))
upper1 <- exp(log(s0)+(mu-sigma*sigma/2)*1:30+qnorm(0.995)*sigma*sqrt(1:30))
df_confint1 <- data.frame(ix = 1:30,
                         l_99 = lower1,
                         u_99 = upper1)

ggplot(df_compare, aes(date, AAPL)) +
  geom_line() +
  geom_line(aes(date, b_forecast), col = 'green') +
  geom_line(aes(date, w_forecast), col = 'red') +
  geom_line(data = df_confint5, aes(ix,l_95), linetype= 'longdash' ) +
  geom_line(data = df_confint5, aes(ix,u_95), linetype= 'longdash') +
  geom_line(data = df_confint1, aes(ix,l_99)) +
  geom_line(data = df_confint1, aes(ix,u_99))

ggplot() +
  geom_line(data = gbm_df ,aes(x=ix, y=price, color=sim)) +
  theme(legend.position = 'none') +
  geom_line(data = df_confint5, aes(ix,l_95), linetype= 'longdash' ) +
  geom_line(data = df_confint5, aes(ix,u_95), linetype= 'longdash') +
  geom_line(data = df_confint1, aes(ix,l_99)) +
  geom_line(data = df_confint1, aes(ix,u_99))

