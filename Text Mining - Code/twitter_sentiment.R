library(rtweet)
library(tm)
library(stringr)
library(xml2)
library(rvest)
library(tidyr)
library(tidytext)
library(wordcloud2)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(forestmangr)

##############------- Extracting Tweets ------##############
## ALL KEYS OMITTED FOR PRIVACY REASONS
twitter_token <- create_token(app = " ",
                              consumer_key = " ",
                              consumer_secret = " ",
                              access_token = " ",
                              access_secret = " ",
                              set_renv = TRUE)

trump.tweets <- get_timeline("@realDonaldTrump", n = 1000)

##############------- Analyzing Tweets ------##############

trump.tweets.organic <- trump.tweets[trump.tweets$is_retweet == FALSE,]
trump.tweets.organic <- subset(trump.tweets.organic, is.na(trump.tweets.organic$reply_to_status_id)) 
head(trump.tweets.organic, 10)
#Number of organic tweets = 370
trump.retweets <- trump.tweets[trump.tweets$is_retweet == TRUE, ]
#Number of retweets = 602
trump.replies <- subset(trump.tweets, !is.na(trump.tweets$reply_to_status_id))
#Number of replies = 25

data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(370, 602, 25)
)

data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
# Rounding the data to two decimal points
data <- round_df(data, 2)
# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  scale_fill_brewer(palette=1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5))+
  ggtitle("Donald Trump's Tweet Distribution")

##############------- Saving Tweets to TXT file ------##############

setwd('C://Users/abhee/Documents/ExcelR/Assignments/Text Mining/Text Mining - Code')
write.table(trump.retweets$text, "trump_retweets.txt", row.names = F)
write.table(trump.tweets.organic$text, "trump_tweets.txt", row.names = F)

tweet.df <- data_frame(text = trump.tweets.organic$text)
retweet.df <- data_frame(text = trump.retweets$text)

##############------- Preprocessing ------##############
### Organic Tweets
tweet.text <- readLines(file.choose())
tweet.docs <- Corpus((VectorSource(tweet.text)))
#inspect(tweet.docs)

tweet.toSpace <- function(x, pattern) gsub(pattern, " ", x)
tweet.docs <- tm_map(tweet.docs, tweet.toSpace, "/")
tweet.docs <- tm_map(tweet.docs, tweet.toSpace, "@")
tweet.docs <- tm_map(tweet.docs, tweet.toSpace, "\\|")
tweet.docs <- tm_map(tweet.docs, tolower)
tweet.docs <- tm_map(tweet.docs, removeNumbers)
tweet.docs <- tm_map(tweet.docs, removePunctuation)
tweet.docs <- tm_map(tweet.docs, removeWords, stopwords("english"))
tweet.docs <- tm_map(tweet.docs, removeWords, c("https", "tco", "amp"))
tweet.docs <- tm_map(tweet.docs, stripWhitespace)
tweet.tdm <- TermDocumentMatrix(tweet.docs)

### Retweets
retweet.text <- readLines(file.choose())
retweet.docs <- Corpus((VectorSource(retweet.text)))
#inspect(retweet.docs)
retweet.toSpace <- function(x, pattern) gsub(pattern, " ", x)
retweet.docs <- tm_map(retweet.docs, retweet.toSpace, "/")
retweet.docs <- tm_map(retweet.docs, retweet.toSpace, "@")
retweet.docs <- tm_map(retweet.docs, retweet.toSpace, "\\|")
retweet.docs <- tm_map(retweet.docs, tolower)
retweet.docs <- tm_map(retweet.docs, removeNumbers)
retweet.docs <- tm_map(retweet.docs, removePunctuation)
retweet.docs <- tm_map(retweet.docs, removeWords, stopwords("english"))
retweet.docs <- tm_map(retweet.docs, removeWords, c("https", "tco", "amp", "tco"))
retweet.docs <- tm_map(retweet.docs, stripWhitespace)
retweet.tdm <- TermDocumentMatrix(retweet.docs)

tweet.tidy <- tidy(tweet.tdm)
tweet.tidy
tweet.tidy.sort <- tweet.tidy %>% count(term, sort = T)

ggplot(data = tweet.tidy.sort[1:13, ], aes(x = reorder(term, -n), y = n)) +
  geom_bar(fill = "yellow", stat = "identity") +
  ggtitle("Total Words Frequency (Organic Tweets)") +
  xlab("Word") +
  ylab("Count") +
  theme_dark()

retweet.tidy <- tidy(retweet.tdm)
retweet.tidy
retweet.tidy.sort <- retweet.tidy %>% count(term, sort = T)
head(retweet.tidy.sort, 20)

ggplot(data = retweet.tidy.sort[1:13, ], aes(x = reorder(term, -n), y = n)) +
  geom_bar(fill = "cyan", stat = "identity") +
  ggtitle("Total Words Frequency (Retweets)") +
  xlab("Word") +
  ylab("Count") +
  theme_dark()

##############------- Sentiment Analysis - Tweets ------##############
#Postive Sentiment
tweet.positive.original <- tweet.tidy %>% 
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  filter(sentiment == "positive") %>% count(term, sort = T)
tweet.positive.original

tweet.positive <- tweet.tidy %>% 
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  filter(!(term == "trump")) %>%
  filter(sentiment == "positive") %>% count(term, sort = T)
tweet.positive

#Postive PLot
ggplot(data = tweet.positive[1:15, ], aes(x = reorder(term, -n), y = n)) +
  geom_bar(fill = "#849931", stat = "identity") +
  ggtitle("Positive Words Frequency (Organic Tweets)") +
  xlab("Word") +
  ylab("Count")

#Positive Sentiment - Word Cloud
tweet.positive %>%
  wordcloud2(size = 1.1, color = 'random-light', 
             backgroundColor = "Black", fontFamily = "Helvetica", shape = 'circle')

#Negative Sentiment
tweet.negative <- tweet.tidy %>% 
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  filter(sentiment == "negative") %>% count(term, sort = T)
tweet.negative

#Negative Plot
ggplot(data = tweet.negative[1:15, ], aes(x = reorder(term, -n), y = n)) +
  geom_bar(fill = "#d11111", stat = "identity") +
  ggtitle("Negative Words Frequency (Organic Tweets)") +
  xlab("Word") +
  ylab("Count")

#Negative Sentiment - Word Cloud
tweet.negative %>%
  wordcloud2(size = 1, color = 'random-dark', 
             backgroundColor = "White", fontFamily = "Helvetica", shape = 'circle')

##############------- Sentiment Analysis - Retweets ------##############

#Postive Sentiment
retweet.positive <- retweet.tidy %>% 
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  filter(!(term == "trump")) %>%
  filter(sentiment == "positive") %>% count(term, sort = T)
retweet.positive

#Postive PLot
ggplot(data = retweet.positive[1:15, ], aes(x = reorder(term, -n), y = n)) +
  geom_bar(fill = "#849931", stat = "identity") +
  ggtitle("Positive Words Frequency (Retweets)") +
  xlab("Word") +
  ylab("Count")

#Positive Sentiment - Word Cloud
retweet.positive %>%
  wordcloud2(size = 1, color = 'random-light', 
             backgroundColor = "Black", fontFamily = "Helvetica", shape = 'circle')

#Negative Sentiment
retweet.negative <- retweet.tidy %>% 
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  filter(sentiment == "negative") %>% count(term, sort = T)
retweet.negative

#Negative Plot
ggplot(data = retweet.negative[1:15, ], aes(x = reorder(term, -n), y = n)) +
  geom_bar(fill = "#d11111", stat = "identity") +
  ggtitle("Negative Words Frequency (Retweets)") +
  xlab("Word") +
  ylab("Count")

#Negative Sentiment - Word Cloud
retweet.negative %>%
  wordcloud2(size = 1.3, color = 'random-dark', 
             backgroundColor = "White", fontFamily = "Helvetica", shape = 'circle')

##############------- N Grams ------##############

tweet.ngrams <- tweet.df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)
head(tweet.ngrams, 20)

retweet.ngrams <- retweet.df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)
head(retweet.ngrams, 20) 

tweet.ngrams.3 <- tweet.df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>%
  count(bigram, sort = T)
head(tweet.ngrams.3, 15)

retweet.ngrams.3 <- retweet.df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) %>%
  count(bigram, sort = T)
head(retweet.ngrams.3, 15) 

##############------- Word Association ------##############

findAssocs(tweet.tdm, terms = "virus", corlimit = 0.3)
findAssocs(tweet.tdm, terms = "business", corlimit = 0.3)
findAssocs(tweet.tdm, terms = "trump", corlimit = 0.3)
findAssocs(tweet.tdm, terms = "jobs", corlimit = 0.3)
findAssocs(tweet.tdm, terms = "america", corlimit = 0.2)
findAssocs(tweet.tdm, terms = "fake", corlimit = 0.3)

findAssocs(retweet.tdm, terms = "coronavirus", corlimit = 0.2)
findAssocs(retweet.tdm, terms = "crisis", corlimit = 0.25)
findAssocs(retweet.tdm, terms = "president", corlimit = 0.2)
findAssocs(retweet.tdm, terms = "workers", corlimit = 0.3)
findAssocs(retweet.tdm, terms = "trump", corlimit = 0.2)
