library(tidyverse)
library(dslabs)
data(gapminder)

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
present_year <- 2010

west <- c("Northern America", "Northern Europe", "Western Europe", "Southern Europe", "Australia and New Zealand")

country_list_1 <- gapminder %>% filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>% filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)

# smooth density plots - variable counts on y-axis
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  geom_density(alpha = 0.2) +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)

# improve the smoothness with bw
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  geom_density(alpha = 0.2, bw = 0.75) +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)

# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region == "Southern Asia" ~ "South Asia",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "South Asia", "Sub-Saharan Africa", "West")))

# Stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)

# Weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population)*2) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group, weight = weight)) +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year~.)
