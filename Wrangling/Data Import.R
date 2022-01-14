library(tidyverse)
library(dslabs)

# see working directory
getwd()

# change your working directory
setwd()

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if the file exists
file.exists(filename)

library(readxl)

# inspect the first 3 lines
read_lines(filename, n_max=3)

# read file in CSV format
dat <- read_csv(filename)
head(dat)

# read using full path
dat <- read_csv(fullpath)
head(dat)

# read using RBase function
dat2 <- read.csv(filename)
class(dat2)

# downloading from internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat1 <- read_csv(url)
head(dat1)

# to have a local copy
download.file(url, "murders.csv")


# 14
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url, n_max=3)
data <- read_csv(url, col_names = FALSE)
str(data)
