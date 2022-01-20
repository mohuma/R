library(tidyverse)
library(Lahman)
library(dslabs)
ds_theme_set()
data(Teams)

# relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# relationship between at bat and runs
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# relationship between errors and wins
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(fielding_error = E/G, win_rate = W/G) %>%
  ggplot(aes(fielding_error, win_rate)) + 
  geom_point(alpha = 0.5)

# relationship between doubles and triples
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(doubles_per_game = X2B/G, triples_per_game = X3B/G) %>%
  ggplot(aes(doubles_per_game, triples_per_game)) + 
  geom_point(alpha = 0.5)

# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
head(GaltonFamilies)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>% 
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

# correlation coefficient
galton_heights %>% summarise(r = cor(father, son)) %>% pull(r)

# compute sample correlation. R is a random variable
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
data.frame(R) %>%
  ggplot(aes(R)) +
  geom_histogram(binwidth = 0.05, color = "black")

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))