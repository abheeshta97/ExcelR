# PRADEEP ABHEESHTA - NEURAL NETWORKS
# CONCRETE


library(neuralnet)  
library(nnet)
library(NeuralNetTools)
library(tidyverse)
library(ggthemes)
library(corrplot)
library(RColorBrewer)

concrete <- read.csv(file.choose())
View(concrete)
str(concrete)
anyNA(concrete)

########### ANALYSIS ##########
# CEMENT
ggplot(data = concrete, aes(x = cement)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Cement")
summary(concrete$cement)

# SLAG
ggplot(data = concrete, aes(x = slag)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Slag")
summary(concrete$slag)

# ASH
ggplot(data = concrete, aes(x = ash)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Ash")
summary(concrete$ash)

# WATER
ggplot(data = concrete, aes(x = water)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Water")
summary(concrete$water)

# WATER
ggplot(data = concrete, aes(x = water)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Water")
summary(concrete$water)

# SUPER PLASTIC
ggplot(data = concrete, aes(x = superplastic)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Superplasticizer")
summary(concrete$superplastic)

# COARSE AGG
ggplot(data = concrete, aes(x = coarseagg)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Coarse Aggregate")
summary(concrete$coarseagg)

# FINE AGG
ggplot(data = concrete, aes(x = fineagg)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Fine Aggregate")
summary(concrete$fineagg)

# AGE
ggplot(data = concrete, aes(x = age)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Age")
summary(concrete$age)

# STRENGTH
ggplot(data = concrete, aes(x = strength)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Concrete Compressive Strength")
summary(concrete$strength)

##### CORRELATION #####

corrplot(cor(concrete), method = 'number', col = brewer.pal(n = 8, name = "Dark2"))


#NORMALIZE FUNCTION
normalize <-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete[,-9],FUN=normalize))
summary(concrete$strength)

concrete_norm <- cbind(concrete_norm,concrete$strength)
colnames(concrete_norm)[9] <- "strength"
concrete_train <- concrete_norm[1:824, ]
concrete_test <- concrete_norm[825:1030, ]

concrete_model <- neuralnet(strength ~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age, 
                            data = concrete_train)
plot(concrete_model, rep = "best")

View(concrete_test)

set.seed(12323)
concrete_model_results <- neuralnet::compute(concrete_model, concrete_test[1:8])
View(concrete_test)
str(model_results)
concrete_pred_strength <- concrete_model_results$net.result
cor(concrete_pred_strength, concrete_test$strength)
plot(concrete_pred_strength,concrete_test$strength)


set.seed(12345)
concrete_model_2 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, 
                              data= concrete_train, hidden = 5, stepmax = 2e5)
plot(concrete_model_2)
concrete_model_2_res <- neuralnet::compute(concrete_model_2, concrete_test[1:8])
concrete_pred_2 <- concrete_model_2_res$net.result
cor(concrete_pred_2,concrete_test$strength)
plot(concrete_pred_2,concrete_test$strength)
