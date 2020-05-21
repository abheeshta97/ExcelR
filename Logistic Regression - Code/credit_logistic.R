library(dplyr)
library(ROCR)
library(ggplot2)
library(reshape2)

credit <- read.csv(file.choose())
View(credit)
credit <- credit[-1]
View(credit)
attach(credit)

summary(credit[-c(1,7,8)])

par(mfrow = c(1,1))
boxplot(age~card, xlab = "Application for Card", ylab = "Age", col = "light yellow")
boxplot(reports~card, xlab = "Application for Card", ylab = "Derogatory Reports", col = "light blue")
boxplot(income~card, xlab = "Application for Card", ylab = "Income (in USD 10,000)", col = "light grey"  )
boxplot(expenditure~card, xlab = "Application for Card", ylab = "Expenditure", col = "orange")
boxplot(share~card, xlab = "Application for Card", ylab = "Share", col = "blue")
boxplot(months~card, xlab = "Application for Card", ylab = "Months Living in Same Address", col = "pink")
boxplot(active~card, xlab = "Application for Card", ylab = "Active", col = "red")

ggplot(data = credit, aes(card, ..count..)) +
  geom_bar(aes(fill = selfemp), position = "dodge") +
  scale_fill_brewer(palette="Dark2")

ggplot(data = credit, aes(card, ..count..)) +
  geom_bar(aes(fill = owner), position = "dodge") +
  scale_fill_brewer(palette="Paired")

credit.fit <- glm(factor(card) ~ reports + age + income + share + expenditure + factor(owner) + factor(selfemp) 
                  + dependents + months + majorcards + active, family = binomial, data = credit )
anova(credit.fit, test = "Chisq")

credit.prob <- predict(credit.fit, type = c("response"), data = credit)
credit.prob

credit.confusion <- table(credit.prob > 0.5, card)
credit.confusion

credit.accuracy <- sum(diag(credit.confusion)/sum(credit.confusion))
credit.accuracy

credit.rocrpred <- prediction(credit.prob, factor(card))
credit.rocrperf <- performance(credit.rocrpred, 'tpr', 'fpr')
plot(credit.rocrperf, colorize = TRUE)

credit.auc <- performance(credit.rocrpred, measure = "auc")
credit.auc@y.values[[1]]

credit.fit_1 <- glm(factor(card) ~ reports + income + share + factor(owner) + factor(selfemp) 
                    + dependents + majorcards, family = binomial, data = credit )


credit.prob_1 <- predict(credit.fit_1, type = c("response"), data = credit)
credit.prob_1


credit.confusion_1 <- table(credit.prob_1 > 0.5, card)
credit.confusion_1

credit.accuracy_1 <- sum(diag(credit.confusion_1)/sum(credit.confusion_1))
credit.accuracy_1


credit.rocrpred_1 <- prediction(credit.prob_1, factor(card))
credit.rocrperf_1 <- performance(credit.rocrpred_1, 'tpr', 'fpr')
plot(credit.rocrperf_1, colorize = TRUE)

credit.auc_1 <- performance(credit.rocrpred_1, measure = "auc")
credit.auc_1@y.values[[1]]
