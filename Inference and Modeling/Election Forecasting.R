library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>% 
  mutate(start = avg - qnorm(0.975)*se, end = avg + qnorm(0.975)*se)

# Computing the posterior mean, standard error, credible interval and probability
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/ (1/sigma^2 + 1/tau^2))
ci <- posterior_mean + c(-qnorm(0.975), qnorm(0.975))*posterior_se
prob <- 1-pnorm(0, posterior_mean, posterior_se) # spread > 0

# Calculating probability of spread > 0 with general bias b = 0.025
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

# top 5 states in electoral votes
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

# Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

# Calculating the posterior mean and posterior standard error
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

# Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

# Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

# Plotting raw percentages across time
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

# 1
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# 2
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
p_hits <- ci_data %>% mutate(hit = actual_spread>lower & actual_spread<upper) %>% summarize(mean(hit))
p_hits

# 3
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p_hits <- ci_data %>% 
  mutate(hit = actual_spread>lower & actual_spread<upper) %>% 
  group_by(pollster) %>% 
  filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))

# 4
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p_hits <- ci_data %>% 
  mutate(hit = actual_spread>lower & actual_spread<upper) %>% 
  group_by(state) %>% 
  filter(n() > 5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))

# 5
head(p_hits)
p_hits %>% arrange(desc(proportion_hits)) %>%
  ggplot(aes(x = state, y = proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# 6
head(cis)
head(ci_data)

errors <- ci_data %>%
  mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
tail(errors)

# 7
errors <- ci_data %>%
  mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

p_hits <- errors %>%
  group_by(state) %>% 
  filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit), n = n())

p_hits %>% arrange(desc(proportion_hits)) %>%
  ggplot(aes(x = state, y = proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# 8
head(errors)
hist(errors$error)
median(errors$error)

# 9
head(errors)
errors %>%
  filter(grade %in% c("A+", "A", "A-", "B+")) %>%
  mutate(state = reorder(state, error, FUN = sum)) %>%
  ggplot(aes(x = state, y = error)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust =1))

# 10
head(errors)
errors %>%
  filter(grade %in% c("A+", "A", "A-", "B+")) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  mutate(state = reorder(state, error, FUN = sum)) %>%
  ggplot(aes(x = state, y = error)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust =1))

# Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)

# expand on Wisconsin result
polls_us_election_2016 %>%
  filter(state =="Wisconsin" &
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  mutate(state = as.character(state)) %>%
  left_join(results_us_election_2016, by = "state") %>%
  mutate(actual = clinton/100 - trump/100) %>%
  summarize(actual = first(actual), avg = mean(spread), 
            sd = sd(spread), n = n()) %>%
  select(actual, avg, sd, n)
  
# Calculate the probability of seeing t-distributed random variables being 
# more than 2 in absolute value when 'df = 3'.
1-pt(2, 3) + pt(-2, 3)

# 2
df <- seq(3,50)
pt_func <- function(n){
  1-pt(2, n) + pt(-2, n)
}
probs <- sapply(df, pt_func)
plot(df, probs)

# 3
library(dslabs)
library(dplyr)
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x)
N <- 15
B <- 10000
set.seed(1)
res <- replicate(B, {
  s <- sample(x, size = N, replace = TRUE)
  between(mu, mean(s)-qnorm(0.975)*(sd(s)/sqrt(N)), mean(s)+qnorm(0.975)*(sd(s)/sqrt(N)))
})
mean(res)

# 4
data(heights)
x <- heights %>% filter(sex == "Male") %>% .$height
mu <- mean(x)
N <- 15
B <- 10000
set.seed(1)
res <- replicate(B, {
  s <- sample(x, size = N, replace = TRUE)
  between(mu, mean(s)-qt(0.975, N-1)*(sd(s)/sqrt(N)), mean(s)+qt(0.975, N-1)*(sd(s)/sqrt(N)))
})
mean(res)

