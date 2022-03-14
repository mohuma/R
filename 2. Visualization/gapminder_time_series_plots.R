library(tidyverse)
library(dslabs)
data(gapminder)

# scatterplot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_point()

# line plot of US fertility by year
gapminder %>%
  filter(country == "United States") %>%
  ggplot(aes(year, fertility)) +
  geom_line()

# fertility time series for two countries
countries <- c("Germany", "South Korea")
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col = country)) +
  geom_line()

# life expectancy time series - lines colored by country and labeled, no legend
countries <- c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1973,1965), y = c(60,72))
gapminder %>%
  filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col=country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position="none")
  
