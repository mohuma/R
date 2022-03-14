library(tidyverse)
library(dslabs)
data(brexit_polls)
options(digits = 3)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500

# 1
E <- N*p
SE <- sqrt(N*p*(1-p))
X_hat <- p
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
d_hat <- 2*X_hat-1
SE_d <- 2*sqrt(X_hat*(1-X_hat)/N)

# 2
head(brexit_polls)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2) 

brexit_polls %>%
  summarize(avg = mean(spread), sd = sd(spread))

brexit_polls %>%
  summarize(avg = mean(x_hat), sd = sd(x_hat))

# 3
brexit_polls[1,] %>%
  mutate(se_hat = sqrt(x_hat*(1-x_hat)/samplesize), 
         lower = x_hat - qnorm(0.975)*se_hat, 
         upper = x_hat + qnorm(0.975)*se_hat)

# 4
actual_spread <- -0.038
june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize), 
         se_spread = 2*se_x_hat, 
         lower = spread - qnorm(0.975)*se_spread, 
         upper = spread + qnorm(0.975)*se_spread,
         hit = actual_spread > lower & actual_spread < upper)
head(june_polls)
nrow(june_polls)
june_polls %>% summarize(sum(sign(lower) != sign(upper))/n())
june_polls %>% summarize(sum(lower > 0)/n())
june_polls %>% summarize(sum(hit)/n())

# 5
p_hits <- june_polls %>%
  group_by(pollster) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))

# 6
june_polls %>%
  ggplot(aes(poll_type, spread))+
  geom_boxplot()

# 7
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2) %>%
  mutate(lower = spread - qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N),
         upper = spread + qnorm(0.975)*2*sqrt(p_hat*(1-p_hat)/N))
combined_by_type

# 9
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

chi <- brexit_hit %>%
  group_by(poll_type, hit) %>% 
  summarize(count = n()) %>%
  spread(poll_type, count)

chisq_test <- chi %>%
  select(-hit) %>%
  chisq.test()
chisq_test$p.value

# 10
48/37
10/32
odds_ratio <- (48/37)/(10/32)

# 11
brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))

# 12
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>%
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method = "loess", span = 0.3)
