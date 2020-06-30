# RANDOM FOREST - COMPANY DATA
# PRADEEP ABHEESHTA

library(caret)
library(ggthemes)
library(corrplot)
library(RColorBrewer)
library(randomForest)

company <- read.csv(file.choose()) #Company_Data.csv
View(company)
dim(company)
anyNA(company)
str(company)

########### ANALYSIS ############

# Sales
ggplot(company, aes(x=Sales, fill = Sales)) + 
  geom_histogram(binwidth=1, color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Sales")

summary(company$Sales)

# Competitor Price
ggplot(company, aes(x=CompPrice)) + 
  geom_histogram(binwidth=1, color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Competitor Price")

ggplot(data= company ,aes(x = CompPrice, fill = CompPrice)) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Competitor Price")

summary(company$CompPrice)

# Income
ggplot(data= company ,aes(x = Income)) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Income")
summary(company$Income)

# Advertising - 663 x 268
ggplot(data= company ,aes(x = Advertising)) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Advertising")
summary(company$Advertising)

# Population
ggplot(data= company ,aes(x = Population)) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Population")
summary(company$Population)

# Price
ggplot(data= company ,aes(x = Price)) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Price")
summary(company$Price)

# ShelveLoc
ggplot(data = company, aes(x = ShelveLoc, fill = ShelveLoc))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() + 
  ggtitle("Shelf Location")
table(company$ShelveLoc)

ggplot(data= company ,aes(x= Sales, y = ShelveLoc, fill = ShelveLoc)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Location in terms of Sales")

# Age
ggplot(data= company ,aes(x = Age)) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Age")
summary(company$Age)

# Education
ggplot(data= company ,aes(x = Education)) +
  geom_boxplot(color="darkblue", fill="lightblue") +
  theme_fivethirtyeight() +
  ggtitle("Education")
summary(company$Education)

# Urban
ggplot(data = company, aes(x = Urban, fill = Urban))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() + 
  ggtitle("Is It in an Urban Location?")
table(company$Urban)

ggplot(data= company ,aes(x= Sales, y = Urban, fill = Urban)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Urban/Rural in terms of Sales")

# US
ggplot(data = company, aes(x = US, fill = US))+
  geom_bar(stat="count", width=0.7, position=position_dodge())+
  theme_fivethirtyeight() + 
  ggtitle("Is it in the US?")
table(company$US)

ggplot(data= company ,aes(x= Sales, y = US, fill = US)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("US/Outside US in terms of Sales")

# Correlation
corrplot(cor(company[c(1:6, 8, 9)]), method = 'number', col=brewer.pal(n=8, name="Dark2"))

#### RANDOM FOREST
high <- ifelse(company$Sales < 10, "No", "Yes")
company_1 <- data.frame(company[-c(1)], high)
View(company_1)
table(company_1$high)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

company_1[c(1:5, 7, 8)] <- normalize(company_1[c(1:5, 7, 8)])  

company_train <- company_1[1:320, ]
company_test <- company_1[321:400, ]

company_RF <- randomForest(high ~ ., data = company_train)
company_RF
plot(company_RF)

company_pred <- predict(company_RF, company_test)
confusionMatrix(company_pred, company_test$high) 

company_RF_1 <- randomForest(high ~ ., data = company_train, ntree = 300, mtry = 3, importance = TRUE,
                    proximity = TRUE)
plot(company_RF_1)
company_pred_1 <- predict(company_RF_1, company_test)
confusionMatrix(company_pred_1, company_test$high) 

company_RF_2 <- randomForest(high ~ ., data = company_train, ntree = 200, 
                             mtry = 3, importance = TRUE, proximity = TRUE)
plot(company_RF_2)
company_pred_2 <- predict(company_RF_2, company_test)
confusionMatrix(company_pred_2, company_test$high) 
