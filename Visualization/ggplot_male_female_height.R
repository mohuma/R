library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
heights %>%
  ggplot(aes(sex,height)) +
  geom_boxplot(coef=3) +
  geom_jitter(width=0.2, alpha=0.2)

heights %>%
  ggplot(aes(height, ..density..)) +
  geom_histogram(binwidth=1, color="black") +
  facet_grid(sex~.)