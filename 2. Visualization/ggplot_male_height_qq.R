library(tidyverse)
library(dslabs)
data(heights)

### basic
p <- heights %>% filter(heights$sex == "Male")
ggplot(p, aes(sample = height)) +
  geom_qq() +
  labs(title = "QQ Plot")

### with mean and SD same as the data
p <- heights %>% filter(heights$sex == "Male")
params <- heights %>% filter(heights$sex == "Male") %>% summarize(mean = mean(height), sd = sd(height))
ggplot(p, aes(sample = height)) +
  geom_abline() + 
  geom_qq(dparams = params) +
  labs(title = "QQ Plot")

### scaled data against the standard normal distribution
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()


