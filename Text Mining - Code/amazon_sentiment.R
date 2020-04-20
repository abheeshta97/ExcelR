#Amazon Reviews - Text Mining
#Product: OnePlus 6T McLaren Edition

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
amazon.url = "https://www.amazon.in/Test-Exclusive-734/product-reviews/B07DJCVTBH/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon.page <- read_html(amazon.url)

amazon.reviews <- html_nodes(amazon.page, '.review-text')
amazon.reviews.text <- html_text(amazon.reviews)
write.table(amazon.reviews.text, "amazon_reviews.txt")

amazon.text <- readLines(file.choose())
amazon.docs <- Corpus((VectorSource(amazon.text)))
inspect(amazon.docs)
amazon.toSpace <- function(x, pattern) gsub(pattern, " ", x)
amazon.docs <- tm_map(amazon.docs, toSpace, "/")
amazon.docs <- tm_map(amazon.docs, toSpace, "@")
amazon.docs <- tm_map(amazon.docs, toSpace, "\\|")
amazon.docs <- tm_map(amazon.docs, tolower)
amazon.docs <- tm_map(amazon.docs, removeNumbers)
amazon.docs <- tm_map(amazon.docs, removeWords, stopwords("english"))
amazon.docs <- tm_map(amazon.docs, removePunctuation)
amazon.docs <- tm_map(amazon.docs, stripWhitespace)
amazon.tdm <- TermDocumentMatrix(amazon.docs)

amazon.tidy <- tidy(amazon.tdm)
amazon.tidy

amazon.reviews.df <- data_frame(text = amazon.reviews.text)
View(amazon.reviews.df)

View(amazon.tidy)

amazon.tidy.sort <- amazon.tidy %>% count(term, sort = T)
head(amazon.tidy.sort, 13)

#Total Words
ggplot(data = amazon.tidy.sort[1:13, ], aes(x = reorder(term, -n), y = n)) +
  geom_bar(fill = "yellow", stat = "identity") +
  ggtitle("Total Words Frequency") +
  xlab("Word") +
  ylab("Count") +
  theme_dark()

amazon.tidy.sort %>%
  wordcloud2(size = 1.3, color = "random-light", 
             backgroundColor = "Black", fontFamily = "Helvetica", shape = 'circle')

###------------------ Sentiment Analysis ------------------###
amazon.positive <- amazon.tidy %>% 
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  filter(sentiment == "positive") %>% count(term, sort = T)
amazon.positive

amazon.negative <- amazon.tidy %>% 
  inner_join(get_sentiments("bing"), by = c("term" = "word")) %>%
  filter(sentiment == "negative") %>% count(term, sort = T)
head(amazon.negative, 15)
amazon.negative

#N-grams
amazon.ngrams <- amazon.reviews.df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = T)
head(amazon.ngrams, 12)

amazon.ngrams.nostop <- amazon.ngrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  filter(!(grepl("[[:digit:]]", word1))) %>%
  filter(!(grepl("[[:digit:]]", word2))) %>%
  count(word1, word2, sort = T) %>%
  unite(bigram, word1, word2, sep = " ")

head(amazon.ngrams.nostop,12)

findAssocs(amazon.tdm, terms = "battery", corlimit = 0.5)
findAssocs(amazon.tdm, terms = "speed", corlimit = 0.5)
findAssocs(amazon.tdm, terms = "display", corlimit = 0.5)
findAssocs(amazon.tdm, terms = "price", corlimit = 0.5)