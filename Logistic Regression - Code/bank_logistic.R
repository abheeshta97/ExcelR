library(ggplot2)
library(dplyr)
library(ROCR)
library(reshape2)
library(fastDummies)

bank <- read.csv(file.choose(), sep=';')
View(bank)

summary(bank[c(1,6,10,12:15)])

## BOXPLOTS

ggplot(bank, aes(y = y, x = age, fill = y)) +
  stat_boxplot(geom ='errorbar') +
  ylab("Term Deposit") +
  geom_boxplot()

ggplot(bank, aes(y = y, x = balance, fill = y)) +
  stat_boxplot(geom ='errorbar') +
  ylab("Term Deposit") +
  geom_boxplot()

ggplot(bank, aes(y = y, x = day, fill = y)) +
  stat_boxplot(geom ='errorbar') +
  ylab("Term Deposit") +
  geom_boxplot()

ggplot(bank, aes(y = y, x = duration, fill = y)) +
  stat_boxplot(geom ='errorbar') +
  ylab("Term Deposit") +
  geom_boxplot()

ggplot(bank, aes(y = y, x = campaign, fill = y)) +
  stat_boxplot(geom ='errorbar') +
  ylab("Term Deposit") +
  geom_boxplot()

ggplot(bank, aes(y = y, x = previous, fill = y)) +
  stat_boxplot(geom ='errorbar') +
  ylab("Term Deposit") +
  geom_boxplot()

## COUNT PLOTS

ggplot(data = bank, aes(job, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = bank, aes(marital, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = bank, aes(education, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = bank, aes(default, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = bank, aes(housing, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = bank, aes(loan, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = bank, aes(contact, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = bank, aes(month, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = bank, aes(poutcome, ..count..)) +
  geom_bar(aes(fill = y), position = "dodge") +
  scale_fill_brewer(palette="Dark2")


bank.dummy <- dummy_cols(bank, select_columns = c("job", "marital", "education", "contact", 
                                                  "month", "poutcome", "default", "housing", "loan", "y"), 
                         remove_first_dummy = TRUE, remove_selected_columns = TRUE)
View(bank.dummy)

attach(bank.dummy)

bank.logit <- glm(y_yes ~ ., family = binomial, data = bank.dummy)

anova(bank.logit, test = "Chisq")

bank.prob <- predict(bank.logit, type = c("response"), data = bank.dummy)
bank.prob

bank.confusion <- table(bank.prob > 0.5, y_yes)
bank.confusion

bank.accuracy <- sum(diag(bank.confusion)/sum(bank.confusion))
bank.accuracy

bank.rocrpred <- prediction(bank.prob, y_yes)
bank.rocrperf <- performance(bank.rocrpred, 'tpr', 'fpr')
plot(bank.rocrperf, colorize = TRUE)

bank.auc <- performance(bank.rocrpred, measure = "auc")
bank.auc@y.values[[1]]

