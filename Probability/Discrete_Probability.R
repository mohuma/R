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
