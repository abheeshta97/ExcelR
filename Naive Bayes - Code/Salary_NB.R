library(tm)
library(e1071)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
library(naivebayes)
library(RColorBrewer)
library(ggthemes)
library(magrittr)

salary_train <- read.csv(file.choose()) #SalaryData_Train.csv
salary_test <- read.csv(file.choose())  #SalaryData_Test.csv

dim(salary_train)
dim(salary_test)

anyNA(salary_train)
anyNA(salary_test)

str(salary_train)
str(salary_test)

#Converting 'educationno' to factor
salary_train$educationno <- as.factor(salary_train$educationno)
salary_test$educationno <- as.factor(salary_test$educationno)

################################# Analyzing the variables in the training set in terms of salary ##########################################

# Salary
ggplot(data = salary_train, aes(x=factor(Salary), fill = Salary))+
  geom_bar(stat="count", width=0.7)+
  theme_fivethirtyeight() +
  ggtitle("Salary")

table(salary_train$Salary)

# Age
ggplot(data= salary_train ,aes(x= Salary, y = age, fill = Salary)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Age in terms of Salary")

# Work Class
ggplot(salary_train, aes(x=factor(workclass), fill = Salary))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  ggtitle("Working Class in terms of Salary")

# Education
ggplot(salary_train, aes(x=factor(education), fill = Salary))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  ggtitle("Education in terms of Salary")

# Marital Status
ggplot(salary_train, aes(x=factor(maritalstatus), fill = Salary))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  ggtitle("Marital Status in terms of Salary")

# Occupation
ggplot(salary_train, aes(x=factor(occupation), fill = Salary))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Occupation in terms of Salary") 

# Relationship
ggplot(salary_train, aes(x=factor(relationship), fill = Salary))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  ggtitle("Relationship in terms of Salary")

# Race
ggplot(salary_train, aes(x=factor(race), fill = Salary))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  ggtitle("Race in terms of Salary")

# Sex
ggplot(salary_train, aes(x=factor(sex), fill = Salary))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() +
  ggtitle("Sex in terms of Salary")

# Capital Gain
ggplot(data= salary_train ,aes(x= Salary, y = capitalgain, fill = Salary)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Capital Gain in terms of Salary")

# Hours Per week 
ggplot(data= salary_train ,aes(x= Salary, y = hoursperweek, fill = Salary)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Hours Per Work in terms of Salary")

# Native
table(salary_train$native)

salary.nb_model <- naiveBayes(data = salary_train ,Salary ~ .)
salary.nb_model

salary_pred <- predict(salary.nb_model, salary_test)
confusionMatrix(salary_pred,salary_test$Salary)

