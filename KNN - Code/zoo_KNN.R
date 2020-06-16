library(class)
library(caret)
library(gmodels)

zoo <- read.csv(file.choose()) #Zoo.csv
View(zoo)
str(zoo)
anyNA(zoo)
dim(zoo)

zoo1 <- zoo[,2:18]

View(zoo1)

# rownames(zoo) <- zoo$animal.name

zoo1$hair <- as.factor(zoo1$hair)
zoo1$feathers <- as.factor(zoo1$feathers)
zoo1$eggs <- as.factor(zoo1$eggs)
zoo1$milk <- as.factor(zoo1$milk)
zoo1$airborne <- as.factor(zoo1$airborne)
zoo1$aquatic <- as.factor(zoo1$aquatic)
zoo1$predator <- as.factor(zoo1$predator)
zoo1$toothed <- as.factor(zoo1$toothed)
zoo1$backbone <- as.factor(zoo1$backbone)
zoo1$breathes <- as.factor(zoo1$breathes)
zoo1$venomous <- as.factor(zoo1$venomous)
zoo1$fins <- as.factor(zoo1$fins)
zoo1$legs <- as.factor(zoo1$legs)
zoo1$tail <- as.factor(zoo1$tail)
zoo1$domestic <- as.factor(zoo1$domestic)
zoo1$catsize <- as.factor(zoo1$catsize)
zoo1$type <- as.factor(zoo1$type)

View(zoo)
table(zoo1$type)

zoo

#create training and test datasets
zoo_train <- zoo1[1:80,]
zoo_test <- zoo1[81:101,]

#Get labels for training and test datasets
zoo_train_labels <- zoo1[1:80,17]
zoo_test_labels <- zoo1[81:101,17]

zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k = 9)

CrossTable(x = zoo_test_labels, y = zoo_pred)

zoo_conf_matrix <- as.matrix(table(Actual = zoo_test_labels, Predicted = zoo_pred))
sum(diag(zoo_conf_matrix))/length(zoo_test_labels)
