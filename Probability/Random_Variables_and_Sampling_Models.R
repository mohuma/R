# define random variable X to be 1 if blue, 0 otherwise
beads <- rep(c("blue", "red"), times = c(2,3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)

# Monte Carlo simulation: Chance of casino losing money on roulette
# method 1
n <- 1000
color <- rep(c("red", "black", "green"), times = c(18,18,2))
#X <- sample(color, n, replace=TRUE)
X <- sample(ifelse(color == "red", -1, 1), n, replace=TRUE)
# method 2
n <- 1000
X <- sample(c(-1,1), n, replace=TRUE, prob=c(9/19, 10/19))

S <- sum(X)

n <- 1000
B <- 10000
roulette_winnings <- function(n){
  X <- sample(c(-1,1), n, replace=TRUE, prob=c(9/19, 10/19))
  sum(X)
}
S <- replicate(B, roulette_winnings(n))
mean(S)
sd(S)

mean(S<=0)

#  plot a histogram of the observed values of S as well as the normal density 
# curve based on the mean and standard deviation of S.
library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

# An old version of the SAT college entrance exam had a -0.25 point penalty for 
# every incorrect answer and awarded 1 point for a correct answer. The quantitative 
# test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose 
# a student chooses answers by guessing for all questions on the test.

# What is the probability of guessing correctly for one question?
right_answer <- 1
wrong_answer <- 4
p <- right_answer/(right_answer+wrong_answer)

# What is the expected value of points for guessing on one question?
a <- 1
b <- -0.25

E <- a*p + b*(1-p)

# What is the expected score of guessing on all 44 questions?
n <- 44
S <- n*E

# What is the standard error of guessing on all 44 questions?
SE <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))

# Use the Central Limit Theorem to determine the probability that a guessing 
# student scores 8 points or higher on the test.
1-pnorm(8,E,SE)

# Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21, sample.kind = "Rounding")

guess <- replicate(10000, {
X <- sample(c(a,b), n, replace = TRUE, prob = c(p, 1-p))
sum(X)
})
mean(guess>=8)

# Suppose that the number of multiple choice options is 4 and that there is no penalty 
# for guessing - that is, an incorrect question gives a score of 0.
right_answer <- 1
wrong_answer <- 3
p <- right_answer/(right_answer+wrong_answer)
a <- 1
b <- 0
E <- a*p + b*(1-p)
n <- 44
S <- n*E

# Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing 
# a range of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
a <- 1
b <- 0
n <- 44
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])

# A casino offers a House Special bet on roulette, which is a bet on five pockets 
# (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other words, 
# a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know 
# the chance of losing money if he places 500 bets on the roulette House Special.

# What is the expected value of the payout for one bet?
a <- 6
b <- -1
p <- 5/38
E <- a*p + b*(1-p)

# What is the standard error of the payout for one bet?
SE <- abs(b-a)*sqrt(p*(1-p))

# What is the expected value of the average payout over 500 bets?
Y <- a*p + b*(1-p)

# What is the standard error of the average payout over 500 bets?
n <- 500
YE <- (abs(b-a)*sqrt(p*(1-p)))/sqrt(n)

# What is the expected value of the sum of 500 bets?
n <- 500
E <- n*(a*p + b*(1-p))

# What is the standard error of the sum of 500 bets?
n <- 500
SE <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))

# Use pnorm() with the expected value of the sum and standard error of the sum 
# to calculate the probability of losing money over 500 bets.
n <- 500
E <- n*(a*p + b*(1-p))
SE <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))
pnorm(0,E,SE)

