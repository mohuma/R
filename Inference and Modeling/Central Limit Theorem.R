library(tidyverse)
library(dslabs)

# Write function called take_sample that takes the proportion of Democrats p and 
# the sample size N as arguments and returns the sample average of Democrats (1) 
# and Republicans (0).
# Calculate the sample average if the proportion of Democrats equals 0.45 and the 
# sample size is 100.

take_sample <- function(p, N) {
  X <- sample(c(1, 0), size = N, replace = TRUE, prob = c(p, 1-p))
  mean(X)
}
p <- 0.45
N <- 100
take_sample(p, N)

# Assume the proportion of Democrats in the population p equals 0.45 and that your 
# sample size N is 100 polled voters. The take_sample function you defined previously 
# generates our estimate,  X_bar.
# Replicate the random sampling 10,000 times and calculate p - X_bar for each random 
# sample. Save these differences as a vector called errors. Find the average of 
# errors and plot a histogram of the distribution.

p <- 0.45
N <- 100
B <- 10000
errors <- replicate(B, p - take_sample(p, N))
mean(errors)

hist(errors)

# The error p - X_bar is a random variable. In practice, the error is not observed 
# because we do not know the actual proportion of Democratic voters, p. However, 
# we can describe the size of the error by constructing a simulation.
# What is the average size of the error if we define the size by taking the absolute 
# value p - X_bar?
p <- 0.45
N <- 100
B <- 10000
errors <- replicate(B, abs(p - take_sample(p, N)))
mean(errors)

# The standard error is related to the typical size of the error we make when 
# predicting. We say size because we just saw that the errors are centered around 0, 
# so thus the average error value is 0. For mathematical reasons related to the 
# Central Limit Theorem, we actually use the standard deviation of errors rather 
# than the average of the absolute values to quantify the typical size. What is 
# this standard deviation of the errors?
p <- 0.45
N <- 100
B <- 10000
errors <- replicate(B, (p - take_sample(p, N))^2)
sqrt(mean(errors))

# The theory we just learned tells us what this standard deviation is going to be 
# because it is the standard error of X_bar. What does theory tell us is the 
# standard error of  X_bar for a sample size of 100?
p <- 0.45
N <- 100
sqrt(p*(1-p)/N)

# In practice, we don't know p, so we construct an estimate of the theoretical 
# prediction based by plugging in X_Bar for p. Compute this estimate.
p <- 0.45
N <- 100
X <- sample(c(1, 0), size = N, replace = TRUE, prob = c(p, 1-p))
X_bar <- mean(X)
sqrt(X_bar*(1-X_bar)/N)

# Earlier we learned that the largest standard errors occur for p =  0.5. 
# Create a plot of the largest standard error for N ranging from 100 to 5,000. 
# Based on this plot, how large does the sample size have to be to have a 
# standard error of about 1%?
N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(N,se)

# If p = 0.45 and N = 100, use the central limit theorem to estimate the 
# probability that X_bar > 0.5
p <- 0.45
N <- 100
1-pnorm(0.5, p, sqrt(p*(1-p)/N))

# Assume you are in a practical situation and you don't know p. Take a sample of 
# size N = 100 and obtain a sample average of X_bar = 0.51. What is the CLT 
# approximation for the probability that your error size is equal or larger than 0.01?
N <-100
X_hat <- 0.51
se_hat <- sqrt(X_hat*(1-X_hat)/N)
1-(pnorm(0.01/se_hat) - pnorm(-0.01/se_hat))

















