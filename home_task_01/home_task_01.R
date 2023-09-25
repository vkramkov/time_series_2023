## ----------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(stargazer)


## ----------------------------------------------------------------------------------------------------------------------
set.seed(20230905)


## ----------------------------------------------------------------------------------------------------------------------
# Сгенерируем временной ряд как реализацию нормального белого шума
ts_length <- 1000
wn_ts <- rnorm(n = ts_length, mean = 5, sd = 2)
wn_table <- tibble(
  time = 1:ts_length,
  white_noise = wn_ts)
rm(ts_length)


## ----------------------------------------------------------------------------------------------------------------------
wn_plot <- ggplot(wn_table, aes(x = time, y = white_noise)) +
  geom_line() + 
  xlab("") + 
  theme_bw()
wn_plot


## ----------------------------------------------------------------------------------------------------------------------
wn_stats <- wn_table |> summarise(mean = mean(white_noise),
                            median = median(white_noise),
                            sd = sd(white_noise),
                            mad = mad(white_noise))
wn_stats


## ----------------------------------------------------------------------------------------------------------------------
plot_wn_density <- ggplot(wn_table, aes(x = white_noise)) +
  geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white", bins = 30) +
  geom_density(alpha = 0.2, fill = "blue") + 
  geom_vline(aes(xintercept = mean(white_noise)),
            color = "blue", size = 1) +
  theme_bw() +
  xlab("") + ylab("")
plot_wn_density


## ----------------------------------------------------------------------------------------------------------------------
wn_plot + 
  geom_hline(aes(yintercept = wn_stats$mean),
             color = "blue", size = 1) +
  geom_hline(aes(yintercept = wn_stats$mean - wn_stats$sd), 
             color = "blue", linetype = "dashed", size = 1) +
    geom_hline(aes(yintercept = wn_stats$mean + wn_stats$sd), 
             color = "blue", linetype = "dashed", size = 1)


## ----------------------------------------------------------------------------------------------------------------------
wn_ts |> acf(lag.max = 20, main = 'Автокорреляционная функция')


## ----------------------------------------------------------------------------------------------------------------------
wn_ts |> acf(lag.max = 20, type = 'partial', main = 'Автокорреляционная функция')


## ----------------------------------------------------------------------------------------------------------------------
wn_table <- wn_table |> 
  mutate(wn_lag_1 = lag(white_noise, 1),
         wn_lag_2 = lag(white_noise, 2))
ggplot(wn_table |> drop_na(), aes(x = wn_lag_1, y = white_noise)) +
  geom_point(alpha = 0.4, size = 2) + 
  geom_smooth(method = 'lm', se = F) +
  theme_classic() + 
  xlab("") + ylab("")


## ----------------------------------------------------------------------------------------------------------------------
ggplot(wn_table |> drop_na(), aes(x = wn_lag_2, y = white_noise)) +
  geom_point(alpha = 0.4, size = 2) + 
  geom_smooth(method = 'lm', se = F) +
  theme_classic() + 
  xlab("") + ylab("")


## ----------------------------------------------------------------------------------------------------------------------
ggplot(wn_table |> drop_na(), aes(x = wn_lag_1, y = white_noise)) +
  geom_point(alpha = 0.4, size = 2) + 
  geom_quantile() +
  theme_classic() + 
  xlab("") + ylab("")


## ----------------------------------------------------------------------------------------------------------------------
ggplot(wn_table |> drop_na(), aes(x = wn_lag_1, y = white_noise)) +
  geom_point(alpha = 0.4, size = 2) + 
  geom_density_2d() +
  theme_classic() + 
  xlab("") + ylab("")


## ----------------------------------------------------------------------------------------------------------------------
# Сгенерируем временной ряд как реализацию нормального белого шума
ts_length <- 1200
burn_length <- 200
ar_ts <- rnorm(n = ts_length, mean = 5, sd = 2)
for (i in 2:ts_length) {
  ar_ts[i] <- 0.9 * ar_ts[i - 1] + ar_ts[i]
}
ar_table <- tibble(
  time = 1:(ts_length - burn_length),
  ar = ar_ts[(burn_length+1):ts_length])
rm(ts_length, burn_length, i)


## ----------------------------------------------------------------------------------------------------------------------
ar_plot <- ggplot(ar_table, aes(x = time, y = ar)) +
  geom_line() + 
  xlab("") + 
  theme_bw()
ar_plot


## ----------------------------------------------------------------------------------------------------------------------
ar_stats <- ar_table |> summarise(mean = mean(ar),                  median = median(ar), sd = sd(ar), mad = mad(ar)) 
ar_stats


## ----------------------------------------------------------------------------------------------------------------------
ar_plot + 
  geom_hline(aes(yintercept = ar_stats$mean),
             color = "blue", size = 1) +   
  geom_hline(aes(yintercept = ar_stats$mean - ar_stats$sd),               color = "blue", linetype = "dashed", size = 1) +
  geom_hline(aes(yintercept = ar_stats$mean + ar_stats$sd),               color = "blue", linetype = "dashed", size = 1)


## ----------------------------------------------------------------------------------------------------------------------
ar_table |> pull(ar) |> acf(lag.max = 20, main = 'Автокорреляционная функция')


## ----------------------------------------------------------------------------------------------------------------------
ar_table |> pull(ar) |> acf(lag.max = 20, main = 'Частная автокорреляционная функция', type = 'partial')


## ----------------------------------------------------------------------------------------------------------------------
ar_table <- ar_table |> 
  mutate(ar_lag_1 = lag(ar, 1),
         ar_lag_2 = lag(ar, 2),
         ar_lag_8 = lag(ar, 8))
ggplot(ar_table |> drop_na(), aes(x = ar_lag_1, y = ar)) +
  geom_point(alpha = 0.4, size = 2) + 
  geom_smooth(method = 'lm', se = F) +
  theme_classic() + 
  xlab("") + ylab("")


## ----------------------------------------------------------------------------------------------------------------------
model <- lm(ar~ar_lag_1, data = ar_table)
stargazer(model, type = "text")


## ----------------------------------------------------------------------------------------------------------------------
ggplot(ar_table |> drop_na(), aes(x = ar_lag_8, y = ar)) +
  geom_point(alpha = 0.4, size = 2) + 
  geom_smooth(method = 'lm', se = F) +
  theme_classic() + 
  xlab("") + ylab("")


## ----------------------------------------------------------------------------------------------------------------------
-log(2)/log(coef(model)[2])


## ----------------------------------------------------------------------------------------------------------------------
ggplot(ar_table |> drop_na(), aes(x = ar_lag_2, y = ar)) +
  geom_point(alpha = 0.4, size = 2) + 
  geom_quantile(size = 1, col = 'black') +
  geom_density2d() +
  theme_classic() + 
  xlab("") + ylab("")


## ----------------------------------------------------------------------------------------------------------------------
# Сгенерируем временной ряд как реализацию нормального белого шума
ts_length <- 1000 
ma_table <- tibble(   
  time = 1:ts_length,   
  noise = runif(n = ts_length, min = -2, max = 6)) |> 
  mutate(ma = noise + 0.95 * lag(noise, 1) + 0.55 * lag(noise, 2) + 10) |> drop_na()
rm(ts_length)

