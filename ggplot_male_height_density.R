library(tidyverse)
library(dslabs)
data(heights)

p <- heights %>% filter(heights$sex == "Male")

ggplot(p, aes(x = height)) +
  geom_density(fill = "blue") +
  labs(title = "Smooth Density Plot", x = "Height in inches")