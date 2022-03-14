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
N <- 50
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

# correlation between various stats in Baseball from Teams dataset
dat <- Teams %>% filter(yearID %in% 1961:2001)
dat %>% summarise(r = cor(R/G, AB/G)) %>% pull(r)
dat %>% summarise(r = cor(W/G, E/G)) %>% pull(r)
dat %>% summarise(r = cor(X2B/G, X3B/G)) %>% pull(r)

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  .$avg
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% 
  mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# plot with regression line
r <- galton_heights %>%
  summarize(r = cor(father, son)) %>%
  .$r
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son = mean(son)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r)

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m * mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

# plot with regression line in standard units
galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

# stratify son heights by the standardized father heights and check for normal approximation
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample = son)) +
  facet_wrap(~ z_father)

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y


set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x
r*s_y/s_x
r*r*100
mu_y + r*((60-mu_x)/s_x)*s_y
