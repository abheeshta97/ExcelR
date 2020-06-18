library(caret)
library(party)
library(ggthemes)
library(corrplot)
library(RColorBrewer)

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

############# DECISION TREE ############

# converting to factors - anything above 10K is high
high <- ifelse(company$Sales < 10, "No", "Yes")
company_1 <- data.frame(company[-c(1)], high)
View(company_1)

# Train and test split

company_train <- company_1[1:300, ]
company_test <- company_1[301:400, ]

# Tree
company_tree <- ctree(data = company_train, high ~ .) 
plot(company_tree)

company_pred <- predict(company_tree,newdata=company_test)
confusionMatrix(company_test$high, company_pred)

