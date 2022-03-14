# finding the interval (x_bar +- MoE) 
# i.e. [x_bar - 1.96SE(X_bar), x_bar + 1.96SE(X_bar)]
# does it contain p?
p <- 0.45
N <- 1000

x <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat*(1-x_hat)/N)

c(x_hat - 1.96*se_hat, x_hat + 1.96*se_hat)

# Monte Carlo simulation for confidence interval
library(dplyr)
p <- 0.45
N <- 1000
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 1.96*SE_hat, X_hat + 1.96*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

# 1
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")

polls <- polls_us_election_2016 %>% filter(state == "U.S." & enddate >= "2016-10-31")
N <- polls$samplesize[1]
X_hat <- polls$rawpoll_clinton[1]/100
se_hat <- sqrt(X_hat*(1-X_hat)/N)
ci <- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)

# 2
head(polls)
pollster_results <- polls %>% 
  mutate(X_hat = rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>%
  select(pollster, enddate, X_hat, se_hat, lower, upper) 

# 3
head(pollster_results)
p <- 0.482
avg_hit <- pollster_results %>%
  mutate(hit = p>lower & p<upper) %>%
  summarize(mean(hit))
avg_hit

# 5
head(polls)
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% 
  mutate(d_hat = (rawpoll_clinton-rawpoll_trump)/100)
N <- polls$samplesize[1]
d_hat <- polls$d_hat[1] # spread
X_hat <- (d_hat +1)/2 # spread = 2p - 1
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N) # SE of spread
ci <- (2*X_hat-1) +c(- qnorm(0.975), qnorm(0.975))*se_hat

# 6
head(polls)
pollster_results <- polls %>% 
  mutate(X_hat = (d_hat +1)/2, se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize), lower = (2*X_hat-1) - qnorm(0.975)*se_hat, upper = (2*X_hat-1) + qnorm(0.975)*se_hat) %>%
  select(pollster, enddate, d_hat, lower, upper) 

# 7
head(pollster_results)
d <- 0.021
avg_hit <- pollster_results %>%
  mutate(hit = d>lower & d<upper) %>%
  summarize(mean(hit))
avg_hit

# 8
head(polls)
d <- 0.021
polls <- polls %>%
  mutate(error = abs(d_hat - d))
polls %>% ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 9
head(polls)
d <- 0.021
polls <- polls %>%
  mutate(error = abs(d_hat - d)) %>%
  group_by(pollster) %>%
  filter(n() >= 5)
polls %>% ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

