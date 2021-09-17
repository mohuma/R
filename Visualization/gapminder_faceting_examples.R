library(tidyverse)
library(dslabs)
data(gapminder)

# facet by continent and year
gapminder %>% filter(year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, col=continent)) +
  geom_point() +
  facet_grid(continent~year)

# facet by year only
gapminder %>% filter(year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, col=continent)) +
  geom_point() +
  facet_grid(.~year)

# facet by year, plots wrapped onto multiple rows
years <- c(1962, 1970, 1980, 1990, 2000, 2012)
continents <- c("Asia", "Europe")
gapminder %>% 
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col=continent)) +
  geom_point() +
  facet_wrap(~year)