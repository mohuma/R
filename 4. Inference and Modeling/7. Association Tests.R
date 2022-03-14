# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(sum) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) 

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

# Two-by-two table for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value for the Lady Tasting Tea problem
fisher.test(tab, alternative = "greater")

# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value

# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

odds_men <- with(two_by_two, (men[2]/sum(men)) / (men[1]/sum(men)))
odds_men

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

odds_women <- with(two_by_two, (women[2]/sum(women)) / (women[1]/sum(women)))
odds_women

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

# p-value and odds ratio responses to increasing sample size
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()


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
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
errors <- ci_data %>%
  mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
head(errors)

totals <- errors %>% 
  filter(grade %in% c("A-", "C-")) %>% 
  select(grade, hit) %>% 
  group_by(grade, hit) %>% 
  summarize(count = n()) %>%
  spread(grade, count)
totals

# 2
chisq_test <- totals %>%
  select(-hit) %>%
  chisq.test()
chisq_test$p.value

# 3
head(totals)
odds_C <- 311/50
odds_A <- 106/26
odds_A/odds_C
