library(tidyverse)
library(dslabs)
data(gapminder)

# add dollars per day variable and define past year
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
past_year <- 1970

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "Black") +
  scale_x_continuous(trans = "log2")

# define Western countries
west <- c("Northern America", "Western Europe", "Northern Europe", "Southern Europe", "Australia and New Zealand")

# facet by West vs developing
gapminder %>%
  filter(year == past_year, !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(.~group)

# facet by West/developing and year
present_year <- 2010

gapminder %>%
  filter(year %in% c(past_year, present_year), !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~group)

# define countries that have data available in both years
country_list_1 <- gapminder %>% filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>% filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~group)

# Boxplots of income in West versus developing world, 1970 and 2010
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = continent)) +
  geom_boxplot() +
  xlab("") +
  scale_y_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(year~.)

# arrange matching boxplots next to each other, colored by year
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot(aes(region, dollars_per_day, fill = factor(year))) +
  geom_boxplot() +
  xlab("") +
  scale_y_continuous(trans = "log2") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))