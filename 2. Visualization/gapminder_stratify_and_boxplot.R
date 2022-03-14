library(tidyverse)
library(dslabs)
data(gapminder)

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

# boxplot of GDP by region in 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day)) +
  geom_boxplot()

# rotate names on x-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# reorder by median income and color by continent
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# log2 scale y-axis
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# add data points
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  