options(digits = 3)
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>%
  ggplot(aes(Age, color = Sex)) +
  geom_density(alpha = 0.2)

titanic %>%
  ggplot(aes(Age, ..count.., color = Sex)) +
  geom_density(alpha = 0.2)

titanic %>%
  ggplot(aes(Age, ..count.., fill = Sex)) +
  geom_density(alpha = 0.2)

t <- titanic %>% filter(!is.na(Age))

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

ggplot(t, aes(sample = Age)) +
  geom_abline() + 
  geom_qq(dparams = params)
  
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())

titanic %>%
  ggplot(aes(Age, ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)


titanic %>%
  filter(Fare > 0.00) %>%
  ggplot(aes(Survived,Fare)) +
  geom_jitter(width=0.2, alpha=0.2) +
  geom_boxplot(coef=3) +
  scale_y_continuous(trans="log2")

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(Age, ..count.., fill=Survived)) +
  geom_density(alpha = 0.2) +
  facet_grid(Sex~Pclass)

