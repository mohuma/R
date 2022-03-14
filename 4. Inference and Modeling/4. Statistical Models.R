library(tidyverse)
library(dslabs)

d <- 0.039 # actual spread
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516) # sample size
p <- (d+1)/2 # d = 2p-1

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - qnorm(0.975)*SE_hat, X_hat + qnorm(0.975)*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals), t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

# another way
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2
polls <- map_df(Ns, function(N) {
  x <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / N)
  list(estimate = 2 * x_hat - 1, 
       low = 2*(x_hat - qnorm(0.975)*se_hat) - 1, 
       high = 2*(x_hat + qnorm(0.975)*se_hat) - 1,
       sample_size = N)
}) %>% mutate(poll = seq_along(Ns))
polls
sum(polls$sample_size)

# Calculating the spread of combined polls.
# estimate of the spread is d_hat
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  pull(avg)

p_hat <- (d_hat+1)/2
moe <- 2*qnorm(0.975)*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
round(d_hat*100,1)
round(moe*100,1)

# generating simulated poll data
library(dslabs)
data("polls_us_election_2016")
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" & (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  pull(d_hat)
d_hat

# compute margin of error
p_hat <- (d_hat+1)/2
moe <- qnorm(0.975) * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe

# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = 0.01)

# Investigating poll data and pollster bias
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())

# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust =1))

# standard error within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarise(se = 2*sqrt(p_hat*(1-p_hat) / median(samplesize)))

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>%
  group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + 
  geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - qnorm(0.975)*se, end = avg + qnorm(0.975)*se)
round(results*100,1)


# 1
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>%
  .$height
mean(x) # mu
sd(x) # sigma

# 2
head(x)
set.seed(1)
N <- 50
X <- sample(x, N, replace = TRUE)
X_bar <- mean(X)
SE_bar <- sd(X)

# 3
# X_bar is a random variable with expected value 'mu' and SE 'sigma / sqrt(N)'

# 4
head(x)
set.seed(1)
N <- 50
X <- sample(x, N, replace = TRUE)
se <- sd(X)/sqrt(N)
ci <- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)

# 5
mu <- mean(x)
set.seed(1)
N <- 50
B <- 10000
res <- replicate(B,{
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval <- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)
  between(mu, mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)
})
mean(res)

# 6
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 
polls %>%
  ggplot(aes(pollster, spread)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust =1))

# 7
# do the two pollsters have different bias? 

# 8
# expected value of Y1j = d+b1

# 13
head(polls)
sigma <- polls %>% group_by(pollster) %>% summarise(s = sd(spread))
sigma

# 15
head(polls)
res <- polls %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread), n = n())
res

estimate <- max(res$avg) - min(res$avg)
estimate

se_hat <- sqrt(((res$s[2]^2)/res$n[2])+((res$s[1]^2)/res$n[1]))
se_hat

ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)
ci

# 16
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
2*(1-pnorm(estimate/se_hat)) # p-value

# 17
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
var <- polls %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread))
var