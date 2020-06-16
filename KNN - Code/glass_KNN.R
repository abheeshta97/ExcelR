library(class)
library(caret)
library(corrplot)
library(gmodels)
library(RColorBrewer)


glass <- read.csv(file.choose()) #glass.csv
View(glass)

anyNA(glass)

dim(glass)

#The records are arranaged according to their type
x <- sample(nrow(glass))
glass_shuffle <- glass[x, ]
View(glass_shuffle)                     

glass_shuffle$Type <- as.factor(glass_shuffle$Type)
glass.std <- cbind(scale(glass_shuffle[,1:9]), glass_shuffle[10])

View(glass.std)

table(glass.std$Type)

corrplot(cor(glass.std[-c(10)]), method = "number", col=brewer.pal(n=8, name="Dark2"))

#create training and test datasets
glass_train <- glass.std[1:171,]
View(glass_train)
glass_test <- glass.std[172:214,]
View(glass_test)

#Get labels for training and test datasets
glass_train_labels <- glass.std[1:171, 10]
glass_test_labels <- glass.std[172:214, 10]

dim(glass_train)
dim(glass_test)

glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k = 13)

CrossTable(x = glass_test_labels, y = glass_pred)
glass_conf_matrix <- as.matrix(table(Actual = glass_test_labels, Predicted = glass_pred))
sum(diag(glass_conf_matrix))/length(glass_test_labels)
