library(tm)
library(e1071)
library(gmodels)
library(caret)
library(naivebayes)

sms_raw <- read.csv(file.choose())#sms_raw_NB.csv
View(sms_raw)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)

View(sms_raw)

anyNA(sms_raw)

table(sms_raw$type)
ggplot(data = sms_raw, aes(x=factor(type), fill = type))+
  geom_bar(stat="count", width=0.5)+
  theme_fivethirtyeight() +
  ggtitle("Ham/Spam")

sms_corpus <- Corpus(VectorSource(sms_raw$text))
sms_corpus <- tm_map(sms_corpus, function(x) iconv(enc2utf8(x), sub = 'byte'))

# Cleaning up the corpus 
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

sms_dtm <- DocumentTermMatrix(corpus_clean)

# Train and Test Split
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170: 5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

#Indicator features for frequent words
#Dictionary of words which are used more
#than 5 times
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

convert_counts <- function(x)
{
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

sms_test_pred <- predict(sms_classifier, sms_test)

prop.table(table(sms_test_pred))
table(sms_test_pred)

CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = F, prop.t = F, prop.r = F,
           dnn = c('predicted', 'actual'))

confusionMatrix(sms_test_pred,sms_raw_test$type)
