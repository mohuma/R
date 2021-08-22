library(tidyverse)
library(dslabs)
library(gridExtra)
data(heights)

p <- heights %>% filter(heights$sex == "Male") %>% ggplot(aes(x=height))

p1 <- p + geom_histogram(binwidth = 1, fill = "blue", color = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", color = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", color = "black")

grid.arrange(p1, p2, p3, ncol=3)

