# Salary - Support Vector Machines 
# Pradeep Abheeshta

library(kernlab)
library(ggplot2)
library(ggthemes)
library(caret)
library(e1071)
library(psych)
library(tidyverse)


salary_train <- read.csv(file.choose()) #SalaryData_Train(1).csv
salary_test <- read.csv(file.choose())  #SalaryData_Test(1).csv

View(salary_train)

## OVERVIEW
dim(salary_train)
dim(salary_test)

anyNA(salary_train)
anyNA(salary_test)

str(salary_train)
str(salary_test)

# Converting 'educationno' to factor
salary_train$educationno <- as.factor(salary_train$educationno)
salary_test$educationno <- as.factor(salary_test$educationno)

##############----- ANALYSIS -----#############

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


##############----- SVM MODELS -----#############

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", 
# "besseldot", "anovadot", "splinedot", "laplacedot"

### 1 - rbfdot
salary_model_1 <- ksvm(Salary ~. , 
                   data = salary_train, kernel = "rbfdot")
salary_pred_1 <- predict(salary_model_1, newdata = salary_test)
mean(salary_pred_1 == salary_test$Salary) # = 0.8520584

### 2 - polydot
salary_model_2 <- ksvm(Salary ~. , 
                   data = salary_train, kernel = "polydot")
salary_pred_2 <- predict(salary_model_2, newdata = salary_test)
mean(salary_pred_2 == salary_test$Salary) # = 0.8461487

### 3 - tanhdot
salary_model_3 <- ksvm(Salary ~. , 
                   data = salary_train, kernel = "tanhdot")
salary_pred_3 <- predict(salary_model_3, newdata = salary_test)
mean(salary_pred_3 == salary_test$Salary) # = 0.6387782

### 4 - vanilladot
salary_model_4 <- ksvm(Salary ~. , 
                   data = salary_train, kernel = "vanilladot")
salary_pred_4 <- predict(salary_model_4, newdata = salary_test)
mean(salary_pred_4 == salary_test$Salary) # = 0.8464143

### 5 - besseldot
salary_model_5 <- ksvm(Salary ~. , 
                   data = salary_train, kernel = "besseldot")
salary_pred_5 <- predict(salary_model_5, newdata = salary_test)
mean(salary_pred_5 == salary_test$Salary) # = 0.7897078

### 6 - anovadot
salary_model_6 <- ksvm(Salary ~. , 
                   data = salary_train, kernel = "anovadot")
salary_pred_6 <- predict(salary_model_6, newdata = salary_test)
mean(salary_pred_6 == salary_test$Salary)

### 7 - splinedot
salary_model_7 <- ksvm(Salary ~. , 
                   data = salary_train, kernel = "splinedot")
salary_pred_7 <- predict(salary_model_7, newdata = salary_test)
mean(salary_pred_7 == salary_test$Salary)

### 8 - laplacedot
salary_model_8 <- ksvm(Salary ~. , 
                   data = salary_train, kernel = "laplacedot")
salary_pred_8 <- predict(salary_model_8, newdata = salary_test)
mean(salary_pred_8 == salary_test$Salary) 

table(salary_pred_1, salary_test$Salary)
