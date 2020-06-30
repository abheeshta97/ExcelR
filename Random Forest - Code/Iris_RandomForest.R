# RANDOM FOREST - IRIS
# PRADEEP ABHEESHTA

library(caret)
library(ggthemes)
library(RColorBrewer)
library(randomForest)

data("iris")
View(iris)
str(iris)
anyNA(iris)

######----- ANALYSIS -----#######

# Sepal Length
ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_boxplot(color="black", fill="#b8a802", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Sepal Length")
summary(iris$Sepal.Length)

# Sepal Width
ggplot(data = iris, aes(x = Sepal.Width)) +
  geom_boxplot(color="black", fill="#b8a802", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Sepal Width")
summary(iris$Sepal.Width)

# Petal Length
ggplot(data = iris, aes(x = Petal.Length)) +
  geom_boxplot(color="black", fill="#b8a802", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Petal Length")
summary(iris$Petal.Length)

# Petal Width
ggplot(data = iris, aes(x = Petal.Width)) +
  geom_boxplot(color="black", fill="#b8a802", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Petal Width")
summary(iris$Petal.Width)

# Species
table(iris$Species)

ggplot(data = iris, aes(x= Sepal.Length, y = Species, fill = Species)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Species in terms of Sepal Length")

ggplot(data = iris, aes(x= Sepal.Width, y = Species, fill = Species)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Species in terms of Sepal Width")

ggplot(data = iris, aes(x= Petal.Length, y = Species, fill = Species)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Species in terms of Petal Length")

ggplot(data = iris, aes(x = Petal.Width, y = Species, fill = Species)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Species in terms of Petal Width")

####### RANDOMIZING ORDER #######
# The dataset needs to be randomized
# since the records arranged according to their species

x <- sample(nrow(iris))
iris_1 <- iris[x, ]
View(iris_1)

##### TRAIN AND TEST #####

iris_train <- iris_1[1:105, ]
iris_test <- iris_1[106:150, ]

######----- RANDOM FOREST 1 -----#######
iris_RF <- randomForest(Species ~ ., data = iris_train)
iris_RF
plot(iris_RF)

iris_pred <- predict(iris_RF, iris_test)
confusionMatrix(iris_pred, iris_test$Species)


######----- RANDOM FOREST 2 -----#######

iris_RF_1 <- randomForest(Species ~ ., data = iris_train, ntree = 200, 
                    mtry = 2, importance = T, proximity = T)
plot(iris_RF_1)

iris_pred_1 <- predict(iris_RF_1, iris_test)
confusionMatrix(iris_pred_1, iris_test$Species)

######----- RANDOM FOREST 3 -----#######

iris_RF_2 <- randomForest(Species ~ ., data = iris_train, ntree = 100, 
                          mtry = 2, importance = T, proximity = T)
plot(iris_RF_2)

iris_pred_2 <- predict(iris_RF_2, iris_test)
confusionMatrix(iris_pred_2, iris_test$Species)
