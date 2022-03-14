library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

head(temp_carbon)
temp_carbon

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  min()

temp_carbon %>%
  filter(year == 2014) %>%
  select(carbon_emissions)

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  min()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(year == 1880) %>%
  select(temp_anomaly)

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year, temp_anomaly)) +
  geom_line() +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")
  

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot() +
  geom_line(aes(year, temp_anomaly), color = "red") +
  geom_line(aes(year, land_anomaly), color = "green") +
  geom_line(aes(year, ocean_anomaly), color = "black") +
  geom_hline(aes(yintercept = 0), col = "blue") +
  ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

head(greenhouse_gases)

greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept=1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")


temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  ggplot(aes(year, carbon_emissions)) +
  geom_line() +
  geom_vline(aes(xintercept=2014))

head(historic_co2)

historic_co2 %>%
  ggplot(aes(year, co2, color=source)) +
  geom_line() +
  scale_x_continuous(limit = c(1000, 2018))
  
  
