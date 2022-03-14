library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

# 1
dat <- Teams_small %>% 
  mutate(HR_per_game = HR/G, R_per_game = R/G)
fitR <- lm(avg_attendance ~ R_per_game, data = dat)
fitR
fitHR <- lm(avg_attendance ~ HR_per_game, data = dat)
fitHR
fitW <- lm(avg_attendance ~ W, data = dat)
fitW
fitY <- lm(avg_attendance ~ yearID, data = dat)
fitY

# 2
cor(dat$R_per_game, dat$W)
cor(dat$HR_per_game, dat$W)

# 3
dat <- dat %>%
  mutate(W_strata = round(W/10, 0)) %>%
  filter(W_strata >= 5 & W_strata <= 10) %>%
  group_by(W_strata) %>%
  filter(n() >= 20) %>%
  ungroup()

dat %>%
  filter(W_strata == 8) 

dat %>%
  group_by(W_strata) %>%
  summarize(slope = cor(avg_attendance, R_per_game)*sd(avg_attendance)/sd(R_per_game)) 

dat %>% ggplot(aes(R_per_game, avg_attendance)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ W_strata)

dat %>%
  group_by(W_strata) %>%
  summarize(slope = cor(avg_attendance, HR_per_game)*sd(avg_attendance)/sd(HR_per_game))

dat %>% ggplot(aes(HR_per_game, avg_attendance)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ W_strata)

# 4
reg <- Teams_small %>% 
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .)
coefs <- tidy(reg, conf.int = TRUE)
coefs

# 5
new_data_2002 <- data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002)
predict(reg, newdata = new_data_2002)

new_data_1960 <- data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960)
predict(reg, newdata = new_data_1960)

# 6
newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         R_per_game = R/G,
         HR_per_game = HR/G)
preds <- predict(reg, newdata)
cor(preds, newdata$avg_attendance)

