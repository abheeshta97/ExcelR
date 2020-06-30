# RANDOM FOREST - FRAUD CHECK
# PRADEEP ABHEESHTA

library(caret)
library(ggthemes)
library(corrplot)
library(RColorBrewer)
library(randomForest)

fraud <- read.csv(file.choose()) #Fraud_check.csv
View(fraud)
dim(fraud)
str(fraud)
anyNA(fraud)

######----- ANALYSIS -----#######

# Taxable Income
ggplot(data = fraud, aes(x = Taxable.Income)) +
  geom_boxplot(color="black", fill="lightblue", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Taxable Income")
summary(fraud$Taxable.Income)

# Undergrad
ggplot(data = fraud, aes(x = Undergrad, fill = Undergrad)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Is The Person an Undergraduate?")
table(fraud$Undergrad)

ggplot(data = fraud, aes(x = Taxable.Income, y = Undergrad, fill = Undergrad)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("UG in terms of Taxable Income")

# Marital Status
ggplot(data = fraud, aes(x = Marital.Status, fill = Marital.Status)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Marital Status")
table(fraud$Marital.Status)

ggplot(data = fraud, aes(x = Taxable.Income, y = Marital.Status, fill = Marital.Status)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Marital Status in terms of Taxable Income")

# City Population
ggplot(data = fraud, aes(x = City.Population)) +
  geom_boxplot(color="black", fill="lightblue", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("City Population")
summary(fraud$City.Population)

# Work Ex
ggplot(data = fraud, aes(x = Work.Experience)) +
  geom_boxplot(color="black", fill="lightblue", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Work Experience")
summary(fraud$Work.Experience)

# Urban
ggplot(data = fraud, aes(x = Urban, fill = Urban)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Is It Urban?")
table(fraud$Urban)

ggplot(data = fraud, aes(x = Taxable.Income, y = Urban, fill = Urban)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Urban in terms of Taxable Income")

#######----- RANDOM FOREST -----########

fraud_risk <- ifelse(fraud$Taxable.Income <= 30000, "Risky", "Good")
fraud_1 <- data.frame(fraud[-c(3)], fraud_risk)
View(fraud_1)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

fraud_1[c(3, 4)] <- normalize(fraud_1[c(3, 4)])  

fraud_train <- fraud_1[1:480, ]
fraud_test <- fraud_1[481:600, ]

####### RANDOM FOREST - 1 ######

fraud_RF <- randomForest(fraud_risk ~ ., data = fraud_train)
fraud_RF
plot(fraud_RF)

fraud_pred <- predict(fraud_RF, fraud_test)
confusionMatrix(fraud_pred, fraud_test$fraud_risk) 

####### RANDOM FOREST - 2 ######

fraud_RF_1 <- randomForest(fraud_risk ~ ., data = fraud_train, ntree = 300, 
                           mtry = 2, importance = T, proximity = T)
plot(fraud_RF_1)

fraud_pred_1 <- predict(fraud_RF_1, fraud_test)
confusionMatrix(fraud_pred_1, fraud_test$fraud_risk) 

####### RANDOM FOREST - 3 ######

fraud_RF_2 <- randomForest(fraud_risk ~ ., data = fraud_train, ntree = 200, mtry = 2, 
                           importance = TRUE, proximity = TRUE)
plot(fraud_RF_2)

fraud_pred_2 <- predict(fraud_RF_2, fraud_test)
confusionMatrix(fraud_pred_2, fraud_test$fraud_risk) 

