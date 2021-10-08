#  Interest rate sampling model
n <- 1000 # number of loans per year
loss_per_foreclosure <- -200000
p <- 0.02 # probability of default
defaults <- sample(c(0,1), n, replace=TRUE, prob=c(1-p,p))
sum(defaults*loss_per_foreclosure)

# Interest rate Monte Carlo simulation
B <- 10000
n <- 1000 # number of loans per year
loss_per_foreclosure <- -200000
p <- 0.02 # probability of default
losses <- replicate(B,{
  defaults <- sample(c(0,1), n, replace=TRUE, prob=c(1-p,p))
  sum(defaults*loss_per_foreclosure) # denoted by S later
})
mean(losses)

# Expected value and standard error of the sum of 1,000 loans
n <- 1000 # number of loans per year
loss_per_foreclosure <- -200000
p <- 0.02 # probability of default
E <- n*(loss_per_foreclosure*p + 0*(1-p))
SE <- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

# break-even on an average
E = 0
loss_per_foreclosure*p + x*(1-p) = 0
x <- - loss_per_foreclosure*p/(1-p)
interest_rate <- x/180000 #interest rate on a 180K loan to break-even

# Calculating interest rate for 1% probability of losing money
# Pr(S<0) = 0.01
l <- -200000
p <- 0.02
n <- 1000
z <- qnorm(0.01)
x <- -l*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))
interest_rate <- x/180000

E <- n*(l*p + x*(1-p)) # total expected profit

# Monte Carlo simulation to double check our theoretical approximations
B <- 10000
n <- 1000 # number of loans per year
l <- -200000 # loss_per_foreclosure
p <- 0.02 # probability of default
z <- qnorm(0.01)
x <- -l*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))
profit <- replicate(B,{
  draws <- sample(c(x,l), n, replace=TRUE, prob=c(1-p,p))
  sum(draws)
})
mean(profit)
mean(profit<0)

###################################################

# Expected value with higher default rate and interest rate
p <- 0.04 # probability of default
loss_per_foreclosure <- -200000
r <- - loss_per_foreclosure*p/(1-p)/180000 # interest rate on a 180K loan to break-even
  
r <- 0.05 # interest rate
x <- r*180000
E <- loss_per_foreclosure*p + x*(1-p) # expected profit per loan

# Calculating number of loans for desired probability (0.01) of losing money
p <- 0.04 # probability of default
l <- -200000 # loss_per_foreclosure
z <- qnorm(0.01)
r <- 0.05 # interest rate
x <- r*180000
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)

# expected profit over n loans
E <- n*(l*p + x * (1-p))

# Monte Carlo simulation with known default probability 
B <- 10000
p <- 0.04 # probability of default
l <- -200000 # loss_per_foreclosure
z <- qnorm(0.01)
r <- 0.05 # interest rate
x <- r*180000
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
profit <- replicate(B,{
  draws <- sample(c(x,l), n, replace=TRUE, prob=c(1-p,p))
  sum(draws)
})
mean(profit)
mean(profit<0)

# Monte Carlo simulation with unknown default probability
# This Monte Carlo simulation estimates the expected profit given an unknown 
# probability of default 0.03 <= p <= 0.05, modeling the situation where an 
# event changes the probability of default for all borrowers simultaneously. 
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, l), n, 
                   prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

# plot the profit
library(tidyverse)
data.frame(profit_in_millions=profit/10^6) %>% 
  ggplot(aes(profit_in_millions)) + 
  geom_histogram(color="black", binwidth = 5)

## 
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

# Use death_prob to determine the death probability of a 50 year old female, p.
p <- death_prob %>% filter(sex == "Female" & age == 50) %>% pull(prob)

# The loss in the event of the policy holder's death is -$150,000 and the gain if 
# the policy holder remains alive is the premium $1,150.
# What is the expected value of the company's net profit on one policy for a 50 year old female?
a <- -150000
b <- 1150
E <- a*p + b*(1-p)

# Calculate the standard error of the profit on one policy for a 50 year old female.
SE <- abs(b-a)*sqrt(p*(1-p))

# What is the expected value of the company's profit over all 1,000 policies for 
# 50 year old females?
n <- 1000
mu <- n*E

# What is the standard error of the sum of the expected value over all 1,000 
# policies for 50 year old females?
sigma <- sqrt(n)*SE

# Use the Central Limit Theorem to calculate the probability that the insurance 
# company loses money on this set of 1,000 policies.
pnorm(0,mu,sigma)

# Use death_prob to determine the probability of death within one year for a 50 year old male.
p <- death_prob %>% filter(sex == "Male" & age == 50) %>% pull(prob)

# Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 
# life insurance policies to be $700,000. Use the formula for expected value of the sum of 
# draws with the following values and solve for the premium b:
n <- 1000
a <- -150000
n*E <- 700000
n*a*p + b*(1-p) <- 700000
b <- ((700000/n)-a*p)/(1-p)

# Using the new 50 year old male premium rate, calculate the standard error of 
# the sum of 1,000 premiums.
SE <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))

# What is the probability of losing money on a series of 1,000 policies to 50 year old males?
pnorm(0,700000,SE)

# a lethal pandemic disease increases the probability of death within 1 year for 
# a 50 year old to .015. Unable to predict the outbreak, the company has sold 
# 1,000 $150,000 life insurance policies for $1,150.
# What is the expected value of the company's profits over 1,000 policies?
p <- 0.015
n <- 1000
a <- -150000
b <- 1150
E <- n*(a*p + b*(1-p))

# What is the standard error of the expected value of the company's profits over 1,000 policies?
SE <- sqrt(n)*abs(b-a)*sqrt(p*(1-p))

# What is the probability of the company losing money?
pnorm(0,E,SE)

# What is the probability of losing more than $1 million?
pnorm(-1000000,E,SE)

# Investigate death probabilities p <- seq(.01, .03, .001)
# What is the lowest death probability for which the chance of losing money exceeds 90%?
n <- 1000
a <- -150000
b <- 1150
p <- seq(.01, .03, .001)
exp_val <- sapply(p, function(p){
  mu <- n * (a*p + b*(1-p))
  sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, mu, sigma)
})

min(p[which(exp_val > 0.9)])

# Investigate death probabilities p <- seq(.01, .03, .0025)
# What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?
n <- 1000
a <- -150000
b <- 1150
p <- seq(.01, .03, .0025)
exp_val <- sapply(p, function(p){
  mu <- n *(a*p + b*(1-p))
  sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1000000, mu, sigma)
})

min(p[which(exp_val > 0.9)])
# another method
n <- 1000
a <- -150000
b <- 1150
p <- seq(.01, .03, .0025)
p_lose_million <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1*10^6, exp_val, se)
})

data.frame(p, p_lose_million) %>%
  filter(p_lose_million > 0.9) %>%
  pull(p) %>%
  min()

# Define a sampling model for simulating the total profit over 1,000 loans with 
# probability of claim p_loss = .015, loss of -$150,000 on a claim, and profit of 
# $1,150 when there is no claim. Set the seed to 25, then run the model once.
# What is the reported profit (or loss) in millions (that is, divided by 10^6)?
set.seed(25, sample.kind = "Rounding")
p_loss <- 0.015
loss <- sample(c(-150000, 1150), 1000, replace=TRUE, prob=c(p_loss,1-p_loss))
sum(loss)/10^6

# Set the seed to 27, then run a Monte Carlo simulation of your sampling model 
# with 10,000 replicates to simulate the range of profits/losses over 1,000 loans.
# What is the observed probability of losing $1 million or more?
set.seed(27, sample.kind = "Rounding")
p_loss <- 0.015
B <- 10000
S <- replicate(B,{
  loss <- sample(c(-150000, 1150), 1000, replace=TRUE, prob=c(p_loss,1-p_loss))
  sum(loss)
})
mean(S<=-1000000)

# Suppose that there is a massive demand for life insurance due to the pandemic, 
# and the company wants to find a premium cost for which the probability of losing 
# money is under 5%, assuming the death rate stays stable at p=0.015.
# Calculate the premium required for a 5% chance of losing money
n <- 1000 
l <- -150000
p <- 0.015 
z <- qnorm(0.05)
x <- -l*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))

# What is the expected profit per policy at this rate?
E <- l*p + x*(1-p)

# What is the expected profit over 1,000 policies?
E <- n*(l*p + x*(1-p))

# Run a Monte Carlo simulation with B=10000to determine the probability of losing 
# money on 1,000 policies given the new premium x, loss on a claim of $150,000, 
# and probability of claim p=0.015. Set the seed to 28 before running your simulation.

# What is the probability of losing money here?
set.seed(28, sample.kind = "Rounding")
B <- 10000
p <- 0.015
l <- -150000 
n <- 1000
profit <- replicate(B,{
  draws <- sample(c(x,l), n, replace=TRUE, prob=c(1-p,p))
  sum(draws)
})
mean(profit<0)

# The company cannot predict whether the pandemic death rate will stay stable. 
# Set the seed to 29, then write a Monte Carlo simulation that for each of B = 10000
# iterations:
# randomly changes  by adding a value between -0.01 and 0.01 with 
# sample(seq(-0.01, 0.01, length = 100), 1)
# uses the new random p to generate a sample of n = 1000 policies with premium x and 
# loss per claim  l = -150000
# The outcome should be a vector of B total profits. Use the results of the Monte 
# Carlo simulation to answer the following three questions.
set.seed(29, sample.kind = "Rounding")
B <- 10000
p <- 0.015
l <- -150000
profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, l), n, 
                   prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})

# What is the expected value over 1,000 policies?
mean(profit)    # expected profit

# What is the probability of losing money?
mean(profit < 0)    # probability of losing money

# What is the probability of losing more than $1 million?
mean(profit < -1000000)    # probability of losing over $1 million
