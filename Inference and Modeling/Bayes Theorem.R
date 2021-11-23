# Monte Carlo simulation of Bayes theorem

prev <- 1/3900 # disease prevalence
N <- 100000
outcome <- sample(c("Disease", "Healthy"), N, replace=TRUE, prob=c(prev,1-prev))

N_D <- sum(outcome == "Disease") # number with disease
N_H <- sum(outcome == "Healthy") # number healthy

accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob=c(accuracy,1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob=c(accuracy,1-accuracy))

table(outcome, test)

# 2
Pr_1 <- 1/8500
Pr_2 <- 1/100
Pr_1 * Pr_2

# 4
Pr_1 <- 1/8500
Pr_2 <- 1/100
Pr_B <- Pr_1 * Pr_2
Pr_A <- 1/1000000
Pr_BA <- 0.50
Pr_AB <- (Pr_A*Pr_BA)/Pr_B

# 6
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)

results <- polls %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread)))
results

# 8
mu <- 0
tau <- 0.01
Y <- results$avg
sigma <- results$se
B <- sigma^2/(sigma^2 + tau^2)
B*mu + (1-B)*Y

# 9
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
sqrt(1/((1/sigma^2)+(1/tau^2)))

# 10
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
ci <- (B*mu + (1-B)*Y) + c(-qnorm(0.975)*se, qnorm(0.975)*se)

# 11
exp_value <- B*mu + (1-B)*Y 
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
pnorm(0,exp_value,se)

# 12
mu <- 0
sigma <- results$se
Y <- results$avg
taus <- seq(0.005, 0.05, len = 100)

p_calc <- function(tau){
  B <- sigma^2 / (sigma^2 + tau^2)
  exp_value <- B*mu + (1-B)*Y 
  se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
  pnorm(0,exp_value,se)
}
ps <- sapply(taus, p_calc)

plot(taus, ps)