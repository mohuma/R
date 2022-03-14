library(tidyverse)
library(dslabs)
data(heights)

p <- heights %>% filter(heights$sex == "Male")

ggplot(p, aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram", x = "Height in inches", y= "Male count")