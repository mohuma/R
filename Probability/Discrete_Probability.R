# create an urn with 2 red, 3 blue
beads <- rep(c("red", "blue"), times=c(2,3))

# view beads object
beads

# sample 1 bead at random
sample(beads,1)

# number of times to draw 1 bead
B <- 10000

# draw 1 bead, B times
events <- replicate(B, sample(beads,1))

# view table of outcome proportions
prop.table(table(events))

# sample 1 bead at random, repeat B times with replacement
events <- sample(beads, B, replace = TRUE)

# view table of outcome proportions
prop.table(table(events))

# Generating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)
library(tidyverse)

# Permutation

# Probability of drawing a second king given that one king is drawn
hands <- permutations(52,2, v=deck)

first_card <- hands[,1]
second_card <- hands[,2]

sum(first_card %in% kings & second_card %in% kings)/sum(first_card %in% kings)

# Combination

# Probability of a natural 21 in blackjack
aces <- paste("Ace", suits)

facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52,2, v=deck)

mean((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard))

#  Monte Carlo simulation of natural 21 in blackjack

B <- 10000

results <- replicate(B, {
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% facecard) | (hand[2] %in% aces & hand[1] %in% facecard)
})

mean(results)

# checking for duplicated birthdays in one 50 person group
bdays <- sample(1:365, 50, replace = TRUE)
any(duplicated(bdays))

# Monte Carlo simulation of the above
B <- 100000
results <- replicate(B, {
  bdays <- sample(1:365, 50, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)

# Monte Carlo simulation of the above with separate function
same_birthday <- function(n){
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
}

results <- replicate(B, same_birthday(50))
mean(results)

# function to compute prob for any group size
compute_prob <- function(n, B=10000){
  results <- replicate(B, same_birthday(n))
  mean(results)
}

# combining above two functions
compute_prob <- function(n, B=10000){
  results <- replicate(B,{
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(results)
}

# perform element-wise operation using sapply
n <- seq(1,60)
prob <- sapply(n, compute_prob)

# plot the probability curve
plot(n,prob)

# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365 # vector of fractions for multiplication rule
  1-prod(prob_unique) # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
n <- seq(1,60)
eprob <- sapply(n, exact_prob)

# plot the (exact) probability curve
lines(n,eprob,col="red")

# Celtics have a 0.4 chance of winning a game. Use Monte Carlo simulation to calculate the probability of Celtics winning at least one game in the 1st 4 games of a 7 games series
# Opponent winning all 4 games = (0.6)^4
# Celtics winning at least one game = 1-((0.6)^4)

B <- 10000

celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  ("win" %in% simulated_games)
})
mean(celtic_wins)

# Monty Hall problem
B <- 10000

monty_hall <- function(strategy){
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  stick == prize_door
  switch <- doors[!doors %in% c(my_pick, show)]
  choice <- ifelse(strategy == "stick", stick, switch)
  choice == prize_door
}

stick <- replicate(B, monty_hall("stick"))
mean(stick)

switch <- replicate(B, monty_hall("switch"))
mean(switch)

# probability of winning 4 out of remaining 6 games after loosing the 1st game in a 7-game series
n <- 6
outcomes <- c(0,1)
l <- rep(list(outcomes), times = n)
possibilities <- expand.grid(l)
results <- rowSums(possibilities)>=4
mean(results)

# (Monte Carlo Simulation) probability of winning 4 out of remaining 6 games after loosing the 1st game in a 7-game series
B <- 10000
results <- replicate(B, {
  match <- sample(c(0,1), 6, replace = TRUE, prob = c(0.5, 0.5))
  sum(match) >= 4
})
mean(results)

# Two teams, A and B, are playing a seven series game series. Team A is better than team B and has a p>0.5 chance of winning each game.
# Use the function sapply to compute the probability, call it Pr of winning for p <- seq(0.5, 0.95, 0.025).
# Then plot the result plot(p, Pr).
p <- seq(0.5, 0.95, 0.025)
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
Pr <- sapply(p, prob_win)
library(tidyverse)
plot(p, Pr)

# Repeat the previous exercise, but now keep the probability that team A wins fixed at p <- 0.75 and compute the probability for different 
# series lengths. For example, wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games.
# Use the seq function to generate a list of odd numbers ranging from 1 to 25.
# Use the function sapply to compute the probability, call it Pr, of winning during series of different lengths.
# Then plot the result plot(N, Pr).
prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}
N <- seq(1, 25, 2)
Pr <- sapply(N, prob_win)
library(tidyverse)
plot(N, Pr)

# In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica 
# and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).

library(gtools)

# How many different ways can the 3 medals(gold, silver, bronze) be distributed across 8 runners?
medals <- permutations(8,3)
nrow(medals)

# How many different ways can the three medals be distributed among the 3 runners from Jamaica?
j_medals <- permutations(3,3)
nrow(j_medals)

# What is the probability that all 3 medals are won by Jamaica?
nrow(j_medals)/nrow(medals)

# (Monte Carlo simulation) probability of winning all three medals by Jamaica sprinters in Olympic 200 m
B <- 10000
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
results <- replicate(B, {
  winner <- sample(runners, 3)
#  (winner[1] == "Jamaica" &  winner[2] == "Jamaica" &  winner[3] == "Jamaica")
  all(winner == "Jamaica")
})
mean(results)

# A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals 
# every day of the year. He doesn't think his current special actually allows that number of choices, but wants 
# to change his special if needed to allow at least 365 choices.

# A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from 
# a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.

library(gtools)

# How many meal combinations are possible with the current menu?
entree <- nrow(combinations(6,1))
side <- nrow(combinations(6,2))
drink <- nrow(combinations(2,1))
entree*side*drink

# The manager has one additional drink he could add to the special.
# How many combinations are possible if he expands his original special to 3 drink options?
entree <- nrow(combinations(6,1))
side <- nrow(combinations(6,2))
drink <- nrow(combinations(3,1))
entree*side*drink

# How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
entree <- nrow(combinations(6,1))
side <- nrow(combinations(6,3))
drink <- nrow(combinations(3,1))
entree*side*drink

# The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, 
# but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order 
# to meet his goal.
# Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 
# 3 drink choices, and a selection of 2 sides from 6 options.
# Use sapply() to apply the function to entree option counts ranging from 1 to 12.
# What is the minimum number of entree options required in order to generate more than 365 combinations?
meal_combinations <- function(n){
  entree <- nrow(combinations(n,1))
  side <- nrow(combinations(6,2))
  drink <- nrow(combinations(3,1))
  meal <- entree*side*drink
}
n <- seq(1,12)
options <- sapply(n,meal_combinations)

data.frame(entrees = 1:12, combos = options) %>%
  filter(combos > 365) %>%
  min(.$entrees)

# The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the 
# number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.
# Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 
# 3 drink choices, and a selection of 2 sides from the specified number of side choices.
# Use sapply() to apply the function to side counts ranging from 2 to 12.
# What is the minimum number of side options required in order to generate more than 365 combinations?
meal_combinations <- function(n){
  entree <- nrow(combinations(6,1))
  side <- nrow(combinations(n,2))
  drink <- nrow(combinations(3,1))
  meal <- entree*side*drink
}
n <- seq(2,12)
options <- sapply(n,meal_combinations)

data.frame(sides = 2:12, combos = options) %>%
  filter(combos > 365) %>%
  min(.$sides)


# Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in 
# dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to 
# people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical 
# characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases 
# and controls grouped by age range (agegp).
df <- esoph
df$newcontrols <- c(40,10,6,5,27,7,4,7,2,1,2,1,1,1,2,60,14,7,8,35,23,14,8,11,6,2,1,3,3,4,46,18,10,4,38,21,15,7,16,14,5,4,4,4,3,4,49,22,12,6,40,21,17,6,18,15,6,4,10,7,3,6,48,14,7,2,34,10,9,13,12,3,1,4,2,1,1,18,6,3,5,3,3,1,1,1,2,1)

# How many groups are in the study?
nrow(df)

# How many cases are there?
all_cases <- sum(df$ncases)
all_cases

# How many controls are there?
all_controls <- sum(df$newcontrols)
all_controls

# What is the probability that a subject in the highest alcohol consumption group is a cancer case?
df %>% mutate(ntotals = ncases + newcontrols) %>% filter(alcgp == "120+") %>% summarize(ncases = sum(ncases), ntotals = sum(ntotals)) %>%
  mutate(p_case = ncases/ntotals) %>% pull(p_case)

# What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
df %>% mutate(ntotals = ncases + newcontrols) %>% filter(alcgp == "0-39g/day") %>% summarize(ncases = sum(ncases), ntotals = sum(ntotals)) %>%
  mutate(p_case = ncases/ntotals) %>% pull(p_case)

# Given that a person is a case, what is the probability that they smoke 10g or more a day?
df %>% filter(tobgp != "0-9g/day") %>% summarize(ncases = sum(ncases)) %>%
  mutate(p_case = ncases/all_cases) %>% pull(p_case)

# Given that a person is a control, what is the probability that they smoke 10g or more a day?
df %>% filter(tobgp != "0-9g/day") %>% summarize(newcontrols = sum(newcontrols)) %>%
  mutate(p_case = newcontrols/all_controls) %>% pull(p_case)

# For cases, what is the probability of being in the highest alcohol group?
df %>% filter(alcgp == "120+") %>% summarize(ncases = sum(ncases)) %>%
  mutate(p_case = ncases/all_cases) %>% pull(p_case)

high_alc_cases <- df %>% filter(alcgp == "120+") %>% pull(ncases) %>% sum()
p_case_high_alc <- high_alc_cases/all_cases
p_case_high_alc

# For cases, what is the probability of being in the highest tobacco group?
df %>% filter(tobgp == "30+") %>% summarize(ncases = sum(ncases)) %>%
  mutate(p_case = ncases/all_cases) %>% pull(p_case)

high_tob_cases <- df %>% filter(tobgp == "30+") %>% pull(ncases) %>% sum()
p_case_high_tob <- high_tob_cases/all_cases
p_case_high_tob

# For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
df %>% filter(tobgp == "30+" & alcgp == "120+") %>% summarize(ncases = sum(ncases)) %>%
  mutate(p_case = ncases/all_cases) %>% pull(p_case)

high_alc_n_tob_cases <- df %>% filter(tobgp == "30+" & alcgp == "120+") %>% pull(ncases) %>% sum()
p_case_high_alc_n_tob <- high_alc_n_tob_cases/all_cases
p_case_high_alc_n_tob

# For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
p_case_high_alc_or_tob <- p_case_high_alc + p_case_high_tob - p_case_high_alc_n_tob
p_case_high_alc_or_tob

# For controls, what is the probability of being in the highest alcohol group?
high_alc_cntls <- df %>% filter(alcgp == "120+") %>% pull(newcontrols) %>% sum()
p_cntl_high_alc <- high_alc_cntls/all_controls
p_cntl_high_alc

# How many times more likely are cases than controls to be in the highest alcohol group?
p_case_high_alc/p_cntl_high_alc

# For controls, what is the probability of being in the highest tobacco group?
high_tob_cntls <- df %>% filter(tobgp == "30+") %>% pull(newcontrols) %>% sum()
p_cntl_high_tob <- high_tob_cntls/all_controls
p_cntl_high_tob

# For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
high_alc_n_tob_cntls <- df %>% filter(tobgp == "30+" & alcgp == "120+") %>% pull(newcontrols) %>% sum()
p_cntl_high_alc_n_tob <- high_alc_n_tob_cntls/all_controls
p_cntl_high_alc_n_tob

# For controls, what is the probability of being in the highest alcohol group or the highest tobacco group?
p_cntl_high_alc_or_tob <- p_cntl_high_alc + p_cntl_high_tob - p_cntl_high_alc_n_tob
p_cntl_high_alc_or_tob

# How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
p_case_high_alc_or_tob/p_cntl_high_alc_or_tob



