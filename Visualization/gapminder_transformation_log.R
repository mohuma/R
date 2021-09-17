library(tidyverse)
library(dslabs)
data(gapminder)

# add dollars per day variable
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# histogram of dollars per day
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled data
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

# repeat histogram with log2 scaled x-axis
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  scale_x_continuous(trans = "log2") +
  geom_histogram(binwidth = 1, color = "black")

