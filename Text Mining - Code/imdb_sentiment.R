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

setwd('C://Users/abhee/Documents/ExcelR/Assignments/Text Mining/Text Mining - Code')

###----------------- Web Scraping ----------------###
#Reviews of the movie "King of Comedy" on IMDb
imdb.url <- "https://www.imdb.com/title/tt0085794/reviews?spoiler=hide&sort=helpfulnessScore&dir=desc&ratingFilter=0"
imdb.page <- read_html(imdb.url)
imdb.page

imdb.reviews <- html_nodes(imdb.page, '.imdb-user-review .text.show-more__control')
imdb.reviews
imdb.reviews.text <- html_text(imdb.reviews)
write.table(imdb.reviews.text, "imdb_reviews.txt")

imdb.reviews.df <- data_frame(text = imdb.reviews.text)
View(imdb.reviews.df)

imdb.tidy <- imdb.reviews.df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

View(imdb.tidy)

imdb.tidy.sort <- imdb.tidy %>% count(word, sort = T)
head(imdb.tidy.sort, 13)

ggplot(data = imdb.tidy.sort[1:13, ], aes(x = reorder(word, -n), y = n)) +
  geom_bar(fill = "yellow", stat = "identity") +
  ggtitle("Total Words Frequency") +
  xlab("Word") +
  ylab("Count") +
  theme_dark()

###------------------ Sentiment Analysis ------------------###
imdb.positive <- imdb.tidy %>% inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment == "positive") %>% count(word, sort = T)
head(imdb.positive, 15)

imdb.negative <- imdb.tidy %>% inner_join(get_sentiments("bing"), by = "word") %>%
  filter(sentiment == "negative") %>% count(word, sort = T)

#Postive PLot
ggplot(data = imdb.positive[1:15, ], aes(x = reorder(word, -n), y = n)) +
  geom_bar(fill = "#849931", stat = "identity") +
  ggtitle("Positive Words Frequency") +
  xlab("Word") +
  ylab("Count")

#Positive Sentiment - Word Cloud
imdb.positive %>%
  wordcloud2(size = 1.3, color = 'random-light', 
             backgroundColor = "Black", fontFamily = "RobotoCondensed-Regular", shape = 'rectangle')

#Negative Plot
ggplot(data = imdb.negative[1:15, ], aes(x = reorder(word, -n), y = n)) +
  geom_bar(fill = "#d11111", stat = "identity") +
  ggtitle("Negative Words Frequency") +
  xlab("Word") +
  ylab("Count")

#Negative Sentiment - Word Cloud
imdb.negative %>%
  wordcloud2(size = 1.3, color = "random-dark", 
             backgroundColor = "White", fontFamily = "RobotoCondensed-Regular", shape = 'circle')

#N-grams
imdb.ngrams <- imdb.reviews.df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)
head(imdb.ngrams, 15)

imdb.ngrams.nostop <- imdb.ngrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
  !word2 %in% stop_words$word) %>%
  filter(!(grepl("[[:digit:]]", word1))) %>%
  filter(!(grepl("[[:digit:]]", word2))) %>%
  count(word1, word2, sort = T) %>%
  unite(bigram, word1, word2, sep = " ")

head(imdb.ngrams.nostop,15)

text <- readLines(file.choose())
docs <- Corpus((VectorSource(text)))
inspect(docs)
toSpace <- function(x, pattern) gsub(pattern, " ", x)
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
imdb.dtm <- TermDocumentMatrix(docs)

findAssocs(imdb.dtm, terms = "comedy", corlimit = 0.5)
findAssocs(imdb.dtm, terms = "crime", corlimit = 0.5)
findAssocs(imdb.dtm, terms = "movie", corlimit = 0.66)
findAssocs(imdb.dtm, terms = "fame", corlimit = 0.6)
findAssocs(imdb.dtm, terms = "celebrity", corlimit = 0.59)
