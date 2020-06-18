library(caret)
library(party)
library(ggthemes)
library(RColorBrewer)

fraud <- read.csv(file.choose()) #Fraud_check.csv
View(fraud)
str(fraud)
anyNA(fraud)

########### ANALYSIS ############

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

############# DECISION TREE ############

fraud_risk <- ifelse(fraud$Taxable.Income <= 30000, "Risky", "Good")
fraud_1 <- data.frame(fraud[-c(3)], fraud_risk)
View(fraud_1)

# Train and Test Split
fraud_train <- fraud_1[1:450, ]
fraud_test <- fraud_1[451:600, ]

# Tree
fraud_tree <- ctree(data = fraud_train, fraud_risk ~ .) 
plot(fraud_tree)


fraud_pred <- predict(fraud_tree, newdata = fraud_test)
confusionMatrix(fraud_test$fraud_risk, fraud_pred)
