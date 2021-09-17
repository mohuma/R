library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
data(heights)
p <- ggplot(murders)
p <- heights %>% ggplot()
murders %>% ggplot(aes(x=population, y=total))+
  geom_point()
murders %>% ggplot(aes(population, total)) +
  geom_label()

murders %>% ggplot(aes(population, total, label=abb)) +
  geom_label(aes(color=region))

p <- heights %>% ggplot(aes(height))
p + geom_density()

heights %>% 
  ggplot(aes(height, fill=sex)) +
  geom_density(alpha=0.2)