library(tidyverse)
library(dslabs)

# inspect the startdate column of 2016 polls data, a Date type
data(polls_us_election_2016)
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state == "U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates,
           month = month(dates),
           day = day(dates),
           year = year(dates))

# extract month labels
month(dates, label = TRUE)

# use parsers to convert strings into any date format
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
ydm(x)
myd(x)
mdy(x)
dmy(x)
dym(x)

Sys.time() # current time
now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12::34::56")
hms(x)

# parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

# look at all available time zones
OlsonNames()



library(dslabs)
library(lubridate)
options(digits = 3) 
data(brexit_polls)

brexit_polls %>% 
  filter(month(startdate) == 04) %>% 
  count()
sum(month(brexit_polls$startdate) == 4)

brexit_polls %>% 
  mutate(week = round_date(enddate, "week")) %>%
  filter(week == "2016-06-12") %>%
  count()
sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

table(weekdays(brexit_polls$enddate))

data(movielens)
head(movielens)

dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))    # count reviews by year
names(which.max(reviews_by_year))    # name of year with most reviews

table(year(as_datetime(movielens$timestamp)))

reviews_by_hour <- table(hour(dates))    # count reviews by hour
names(which.max(reviews_by_hour))    # name of hour with most reviews

table(hour(as_datetime(movielens$timestamp)))
