library(tidyverse)
options(digits = 3)

library(dslabs)
data("brexit_polls")

head(brexit_polls)

p <- 0.481
d <- 2*p - 1
N <- 1500

E <- p * N
SE <- sqrt(N) * sqrt(p * (1-p))
X_hat <- p
se_X_hat <- sqrt(X_hat * (1-X_hat) / N)

polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1) / 2) %>%
  summarise(avg_spread = mean(spread), sd_spread = sd(spread), avg = mean(x_hat), sd = sd(x_hat))

brexit_polls[1,]
X_hat_1 <- brexit_polls[1,]$remain
SE_1 <- sqrt(X_hat_1 * (1-X_hat_1) / brexit_polls[1,]$samplesize)
lower <- X_hat_1 - qnorm(0.975) * SE_1
upper <- X_hat_1 + qnorm(0.975) * SE_1

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1) / 2)
june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat * (1-x_hat) / samplesize)) %>%
  mutate(se_spread = 2*se_x_hat) %>%
  mutate(lower = spread - qnorm(0.975) * se_spread, upper = spread + qnorm(0.975) * se_spread) %>%
  mutate(hit = lower <= d & upper >= d)
mean(june_polls$lower <= 0 & june_polls$upper >= 0)
mean(june_polls$lower >= 0)
mean(june_polls$hit)

june_polls %>%
  group_by(pollster) %>%
  summarise(hit_rate = mean(hit), num_poll = n()) %>%
  arrange(hit_rate)

june_polls %>%
  group_by(poll_type) %>%
  ggplot(aes(poll_type, spread)) +
  geom_boxplot() +
  geom_point()

june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_ptype = sqrt(p_hat * (1-p_hat) / N) * 2,
            lower = spread - qnorm(0.975) * se_ptype,
            upper = spread + qnorm(0.975) * se_ptype)
