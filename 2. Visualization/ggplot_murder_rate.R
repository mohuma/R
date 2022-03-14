library(tidyverse)
library(dslabs)
library(ggrepel)
library(ggthemes)
data(murders)

r <- murders %>% summarize(rate = sum(total)/sum(population)*10^6) %>% .$rate

ggplot(murders, aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(color=region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "US Murder Rate 2010", x = "Population in millions (log scale)", y = "Murder count (log scale)") +
  theme_economist() +
  scale_color_discrete(name = "Region")

