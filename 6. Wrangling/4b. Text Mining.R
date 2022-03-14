library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

# In general, we can extract data directly from Twitter using the rtweet package. 
# However, in this case, a group has already compiled data for us and made it 
# available at https://www.thetrumparchive.com/

url <- 'https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

# For convenience we include the result of the code above in the dslabs package
library(dslabs)
data("trump_tweets")

# This is data frame with information about the tweet
head(trump_tweets)

# The variables that are included are
names(trump_tweets)

# The help file ?trump_tweets provides details on what each variable represents. 
# The tweets are represented by the text variable:
trump_tweets %>% select(text) %>% head

# the source variable tells us the device that was used to compose and upload each tweet
trump_tweets %>% count(source) %>% arrange(desc(n))

# use extract to remove the Twitter for part of the source and filter out retweets
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source)

# focus on what was tweeted between the day Trump announced his campaign and election day
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# plot the tweet time vs proportion of tweets at each hour for each device
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# text as data
library(tidytext)

example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

# look at a quick example with a tweet number 3008
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

# define a regex that captures twitter character
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# use the unnest_tokens() function with the regex option and appropriately extract the hashtags and mentions
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# remove the links to pictures
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# extract the words for all the tweets
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

# count and arrange words
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

# The tidytext package has database of these commonly used words, referred to as stop words, in text mining
stop_words

# filter out rows representing stop words
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word )

# informative set of top 10 tweeted words
tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

# filter out rows representing stop words and unwanted characters
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

# For each word we want to know if it is more likely to come from an Android tweet or an iPhone tweet. 
# We previously introduced the odds ratio, a summary statistic useful for quantifying these differences. 
# For each device and a given word, let's call it y, we compute the odds or the ratio between the 
# proportion of words that are y and not y and compute the ratio of those odds. Here we will have 
# many proportions that are 0 so we use the 0.5 correction.
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

# impose a filter based on the total frequency
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

# sentiment analysis
# The tidytext package includes several maps or lexicons in the object sentiments
sentiments 

# the bing lexicon divides words into positive and negative. 
# We can see this using the tidytext function get_sentiments()
get_sentiments("bing")

# The AFINN lexicon assigns a score between -5 and 5, with -5 the most negative and 5 the most positive
get_sentiments("afinn")

# The loughran and nrc lexicons provide several different sentiments
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

# For the analysis here we are interested in exploring the different sentiments 
# of each tweet, so we will use the nrc lexicon
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

# combine the words and sentiments using inner_join(), which will only keep words associated with a sentiment. 
# Here are 10 random words extracted from the tweets
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

# count and compare the frequencies of each sentiment appears for each device
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

# more words were used on the Android than on the phone
tweet_words %>% group_by(source) %>% summarize(n = n())

# for each sentiment we can compute the odds of being in the device: proportion 
# of words with sentiment versus proportion of words without and then compute the 
# odds ratio comparing the two devices
sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

# compute, for each sentiment, an odds ratio and confidence interval. We will add 
# the two values we need to form a two-by-two table and the odds ratio
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

# A graphical visualization shows some sentiments that are clearly overrepresented
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip()

# exploring which specific words are driving these differences
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

# make a graph
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata

gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

gutenberg_works(title == "Pride and Prejudice")

pnp <- gutenberg_download(1342, mirror="http://gutenberg.readingroo.ms/")
words <- pnp %>%
  unnest_tokens(word, text)
nrow(words)

words <- words %>%
  filter(!word %in% stop_words$word)
nrow(words)

words <- words %>%
  filter(!str_detect(word, "\\d+"))
nrow(words)

words %>%
  count(word) %>%
  filter(n > 100) %>%
  nrow()

words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(word)

words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(n)

afinn <- get_sentiments("afinn") 

afinn_sentiments <- inner_join(afinn, words)
nrow(afinn_sentiments)

mean(afinn_sentiments$value > 0)

sum(afinn_sentiments$value == 4)




