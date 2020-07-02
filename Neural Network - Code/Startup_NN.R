
# PRADEEP ABHEESHTA - NEURAL NETWORKS
# 50 STARTUPS

library(neuralnet)  
library(nnet)
library(NeuralNetTools)
library(tidyverse)
library(plyr)
library(fastDummies)
library(ggthemes)
library(corrplot)
library(RColorBrewer)

startup <- read.csv(file.choose())
View(startup)
str(startup)
anyNA(startup)

######------- ANALYSIS ------######
# RD SPEND
ggplot(data = startup, aes(x = R.D.Spend)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("R&D Spending")
summary(startup$R.D.Spend)

# ADMIN
ggplot(data = startup, aes(x = Administration)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Admin Spending")
summary(startup$Administration)

# MARKETING SPEND
ggplot(data = startup, aes(x = Marketing.Spend)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Marketing Spending")
summary(startup$Marketing.Spend)

# STATE
ggplot(data = startup, aes(x = State, fill = State)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("States")
table(startup$State)

# PROFIT
ggplot(data = startup, aes(x = Profit)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Profits")
summary(startup$Profit)

######------- CORRELATION -----######
corrplot(cor(startup[-c(4)]), method = 'number', col = brewer.pal(n = 8, name = "Dark2"))


######------ NORMALIZING -----######

startup$State <- as.numeric(revalue(startup$State,
                                     c("New York" = "0", "California" = "1",
                                       "Florida" = "2")))

startup_norm <- startup
View(startup_norm)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

startup_norm$R.D.Spend <- normalize(startup_norm$R.D.Spend)
startup_norm$Administration <- normalize(startup_norm$Administration)
startup_norm$Marketing.Spend <- normalize(startup_norm$Marketing.Spend)
startup_norm$State <- normalize(startup_norm$State)


startup_train <- startup_norm[1:35, ]
startup_test <- startup_norm[36:50, ]

colnames(startup_train)

######----- NN MODEL - 1 -----#######
startup_form <- paste("Profit",  paste(colnames(startup_norm[-5]),collapse ="+"), sep="~")
startup_model <- neuralnet(formula = startup_form, data = startup_train, hidden = 5, stepmax = 3e5, threshold = 0.4)
plot(startup_model, rep = "best")

set.seed(12323)
startup_model_results <- neuralnet::compute(startup_model, startup_test[-c(5)])
startup_pred <- startup_model_results$net.result
cor(startup_pred, startup_test$Profit)
plot(startup_pred,startup_test$Profit)

######----- NN MODEL - 2 -----#####
startup_model_1 <- neuralnet(formula = startup_form, data = startup_train, 
                             hidden = 7, stepmax = 3e5, threshold = 0.4)
plot(startup_model_1, rep = "best")

set.seed(12323)
startup_model_results_1 <- neuralnet::compute(startup_model_1, startup_test[-c(5)])
startup_pred_1 <- startup_model_results_1$net.result
cor(startup_pred_1, startup_test$Profit)
plot(startup_pred_1,startup_test$Profit)

######----- NN MODEL - 3 -----#####
startup_model_2 <- neuralnet(formula = startup_form, data = startup_train, 
                             hidden = 12, stepmax = 3e5, threshold = 0.4, learningrate = 0.1)
plot(startup_model_2, rep = "best")

set.seed(12323)
startup_model_results_2 <- neuralnet::compute(startup_model_2, startup_test[-c(5)])
startup_pred_2 <- startup_model_results_2$net.result
cor(startup_pred_2, startup_test$Profit)
plot(startup_pred_2,startup_test$Profit)

######----- NN MODEL - 4 -----#####
startup_model_3 <- neuralnet(formula = startup_form, data = startup_train)
plot(startup_model_3, rep = "best")

set.seed(12323)
startup_model_results_3 <- neuralnet::compute(startup_model_3, startup_test[-c(5)])
startup_pred_3 <- startup_model_results_3$net.result
cor(startup_pred_3, startup_test$Profit)
plot(startup_pred_3, startup_test$Profit)


