
# PRADEEP ABHEESHTA - NEURAL NETWORKS
# FOREST FIRES

library(neuralnet)  
library(nnet)
library(NeuralNetTools)
library(tidyverse)
library(fastDummies)
library(ggthemes)
library(corrplot)
library(RColorBrewer)

forest_fire <- read.csv(file.choose()) #forestfires.csv

## OVERVIEW
View(forest_fire)
dim(forest_fire)
colnames(forest_fire)
anyNA(forest_fire)

##############----- ANALYSIS -----#############
# SIZE CATEGORY
ggplot(data = forest_fire, aes(x = size_category, fill = size_category)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Size of Fire")
table(forest_fire$size_category)

# MONTH
ggplot(data = forest_fire, aes(x = month, fill = month)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Month")
table(forest_fire$month)

# DAY 
ggplot(data = forest_fire, aes(x = day, fill = day)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Day")
table(forest_fire$day)

# FFMC 
ggplot(data = forest_fire, aes(x = FFMC)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("FFMC")
summary(forest_fire$FFMC)

# DMC 
ggplot(data = forest_fire, aes(x = DMC)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("DMC")
summary(forest_fire$DMC)

# DC
ggplot(data = forest_fire, aes(x = DC)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("DC")
summary(forest_fire$DC)

# ISI
ggplot(data = forest_fire, aes(x = ISI)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("ISI")
summary(forest_fire$ISI)

# Temperature
ggplot(data = forest_fire, aes(x = temp)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +2
theme_fivethirtyeight() +
  ggtitle("Temperature")
summary(forest_fire$temp)

# RH
ggplot(data = forest_fire, aes(x = RH)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("RH")
summary(forest_fire$RH)

# Wind
ggplot(data = forest_fire, aes(x = wind)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Wind")
summary(forest_fire$wind)

# Rain
ggplot(data = forest_fire, aes(x = rain)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Rain")
summary(forest_fire$rain)

# Area
ggplot(data = forest_fire, aes(x = area)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Area")
summary(forest_fire$area)

## CORRPLOT
corrplot(cor(forest_fire[c(3:11)]), method = 'number', col = brewer.pal(n = 8, name = "Dark2"))

##############----- NORMALIZING/DUMMY -----#############
FF_norm <- forest_fire[-c(1,2)]
FF_norm <- dummy_cols(FF_norm, select_columns = "size_category", 
           remove_first_dummy = T, remove_selected_columns = T)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
FF_norm$RH   = normalize(FF_norm$RH)
FF_norm$wind = normalize(FF_norm$wind)
FF_norm$temp = normalize(FF_norm$temp)
FF_norm$FFMC = normalize(FF_norm$FFMC)
FF_norm$DMC = normalize(FF_norm$DMC)
FF_norm$DC = normalize(FF_norm$DC)
FF_norm$ISI = normalize(FF_norm$ISI)

View(FF_norm)

FF_train <- FF_norm[1:413, ]
FF_test <- FF_norm[414:517, ]

##############----- NN MODEL -----#############

FF_model <- neuralnet(area ~ ., data = FF_train, hidden = 5, stepmax = 1.5e5)
plot(FF_model, rep = "best")

set.seed(12323)
FF_model_results <- neuralnet::compute(FF_model, FF_test[-c(9)])
FF_pred_1 <- FF_model_results$net.result
cor(FF_pred_1, FF_test$area)
plot(FF_pred_1,FF_test$area)
