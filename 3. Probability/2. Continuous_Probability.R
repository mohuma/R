library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>% pull(height)
F <- function(a) mean(x<=a)

# probability that a random student height is greater than 70.5 inches
1-F(70.5)
# theoretical calculation of the above using normal approximation of male heights
1-pnorm(70.5, mean(x), sd(x))

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# Plotting the probability density for the normal distribution
library(tidyverse)
x <- seq(-4, 4, length=100)
data.frame(x, f = dnorm(x)) %>% 
  ggplot(aes(x,f)) +
  geom_line()

# define x as male heights from dslabs data
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

# Monte Carlo simulation of tallest person over 7 feet
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)
  max(simulated_data)
})
mean(tallest >= 7*12)

# Assume the distribution of female heights is approximated by a normal distribution 
# with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female 
# at random, what is the probability that she is 5 feet or shorter?

pnorm(5*12, 64, 3)

# Assume the distribution of female heights is approximated by a normal distribution 
# with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female 
# at random, what is the probability that she is 6 feet or taller?

1-pnorm(72, 64, 3)

# Assume the distribution of female heights is approximated by a normal distribution 
# with a mean of 64 inches and a standard deviation of 3 inches. If we pick a female 
# at random, what is the probability that she is between 61 and 67 inches?

pnorm(67, 64, 3) - pnorm(61, 64, 3)

# Repeat the previous exercise, but convert everything to centimeters. That is, 
# multiply every height, including the standard deviation, by 2.54. What is the answer now?

pnorm(67*2.54, 64*2.54, 3*2.54) - pnorm(61*2.54, 64*2.54, 3*2.54)

# Imagine the distribution of male adults is approximately normal with an average of 
# 69 inches and a standard deviation of 3 inches. How tall is a male in the 99th percentile?

qnorm(0.99, 69, 3)

# The distribution of IQ scores is approximately normally distributed. The average is 100 
# and the standard deviation is 15. Suppose you want to know the distribution of the highest 
# IQ across all graduating classes if 10,000 people are born each in your school district. 
# Run a Monte Carlo simulation with B=1000 generating 10,000 IQ scores and keeping the highest. 
# Make a histogram
B <- 1000
highestIQ <- replicate(B, {
  simulated_data <- rnorm(10000, 100, 15)
  max(simulated_data)
})
hist(highestIQ)


# ACT scores
# 
set.seed(16, sample.kind = "Rounding")

# Generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7. 
# Save these values as act_scores.

act_scores <- rnorm(10000, 20.9, 5.7)

# What is the mean of act_scores?
mean(act_scores)

# What is the standard deviation of act_scores?
sd(act_scores)

# In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores >= 36)

# In act_scores, what is the probability of an ACT score greater than 30?
mean(act_scores > 30)

# In act_scores, what is the probability of an ACT score less than or equal to 10?
mean(act_scores <= 10)

# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the 
# value of the probability density function over x given a mean of 20.9 and standard 
# deviation of 5.7; save the result as f_x. Plot x against f_x.

library(tidyverse)
x <- seq(1, 36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

# Convert act_scores to Z-scores.
z <- (act_scores - mean(act_scores))/sd(act_scores)

# What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
mean(z > 2)

# What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
z <- 2
x <- z*sd(act_scores) + mean(act_scores)

# Use qnorm() to determine the 97.5th percentile of normally distributed data with 
# the mean and standard deviation observed in act_scores.
# What is the 97.5th percentile of act_scores?

qnorm(0.975, mean(act_scores), sd(act_scores))

# Write a function that takes a value and produces the probability of an ACT score less than 
# or equal to that value (the CDF). Apply this function to the range 1 to 36.

x <- seq(1, 36)
f <- function(x){
  mean(act_scores <= x)
}
cdf <- sapply(x, f)

# What is the minimum integer score such that the probability of that score or lower is at least .95?
#data.frame(score = x, cdf = cdf) %>% filter(cdf > 0.95) %>% min(.$score)
min(which(cdf >= .95))

# Use qnorm() to determine the expected 95th percentile, the value for which the probability of 
# receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7.
qnorm(0.95, 20.9, 5.7)

# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th 
# percentiles of the act_scores data. Save these as sample_quantiles
# In what percentile is a score of 26?
p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
#sample_quantiles < 26
names(sample_quantiles[max(which(sample_quantiles < 26))])

# Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) 
# with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing 
# sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qqplot(theoretical_quantiles, sample_quantiles)

