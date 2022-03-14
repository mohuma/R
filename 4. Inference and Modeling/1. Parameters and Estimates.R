# a function for taking a random draw of size  from the urn 
library(tidyverse)
library(dslabs)
take_poll(25) 

# Write a line of code that calculates the standard error se of a sample average 
# when you poll 25 people in the population. Generate a sequence of 100 proportions 
# of Democrats p that vary from 0 (no Democrats) to 1 (all Democrats).

# Plot se versus p for the 100 different proportions.
N <- 25
p <- seq(0, 1, length.out = 100)
se <- sqrt(p*(1-p))/sqrt(N)
plot(p, se)

# Using the same code as in the previous exercise, create a for-loop that generates 
# three plots of p versus se when the sample sizes equal N=25, N=100, and N=1000.
p <- seq(0, 1, length = 100)
sample_sizes <- c(25, 100, 1000)
for (N in sample_sizes){
  se <- sqrt(p*(1-p))/sqrt(N)
  plot(p, se, ylim = c(0, 0.1))
}

# p = proportion of blue beads in an urn
# Computing the probability of x_bar being within .01 of p
# blue beads = 12; red beads = 13 in a draw of 25 beads
x_hat <- 0.48
se <- sqrt(x_hat * (1-x_hat) / 25)
pnorm(0.01/se) - pnorm(-0.01/se)

# Margin or error
2*se

# Monte Carlo simulation using a set value of p

p <- 0.45
N <- 1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(X)

B <- 10000
x_hat <- replicate(B,{
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(X)
})
mean(x_hat)
sd(x_hat)

# Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

# Plotting margin of error in an extremely large poll over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
MOE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, MOE = MOE) %>%
  ggplot(aes(p, MOE)) +
  geom_line()
