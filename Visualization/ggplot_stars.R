library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3) 

mean(stars$magnitude)
sd(stars$magnitude)

stars %>%
  ggplot(aes(magnitude)) +
  geom_density()

stars %>%
#  filter(star %in% c("Antares", "Castor", "Mirfak", "Polaris", "vanMaanen'sStar")) %>%
  ggplot(aes(temp, magnitude, label = star)) +
  geom_point() +
  scale_x_reverse() +
  scale_y_reverse() +
  geom_text_repel()

stars %>%
  ggplot(aes(log10(temp), magnitude)) +
  geom_point(aes(color=type)) +
  scale_x_reverse() +
  scale_y_reverse()
 