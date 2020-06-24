# Forest Fires - Support Vector Machines 
# Pradeep Abheeshta

library(kernlab)
library(ggplot2)
library(ggthemes)
library(caret)
library(e1071)
library(psych)
library(tidyverse)

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

ggplot(data = forest_fire, aes(x = month, fill = size_category)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Month in Terms of Size of Area")

# DAY 
ggplot(data = forest_fire, aes(x = day, fill = day)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Day")
table(forest_fire$day)

ggplot(data = forest_fire, aes(x = day, fill = size_category)) +
  geom_bar(stat="count", width=0.5, position=position_dodge()) +
  theme_fivethirtyeight() + 
  ggtitle("Day in Terms of Size of Fire")

# FFMC 
ggplot(data = forest_fire, aes(x = FFMC)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("FFMC")
summary(forest_fire$FFMC)

ggplot(data = forest_fire, aes(x = FFMC, y = size_category, fill = size_category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("FFMC in Terms of Size of Area")

by(forest_fire$FFMC, forest_fire$size_category, summary)

# DMC 
ggplot(data = forest_fire, aes(x = DMC)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("DMC")
summary(forest_fire$DMC)

ggplot(data = forest_fire, aes(x = DMC, y = size_category, fill = size_category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("DMC in Terms of Size of Area")

by(forest_fire$DMC, forest_fire$size_category, summary)

# DC
ggplot(data = forest_fire, aes(x = DC)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("DC")
summary(forest_fire$DC)

ggplot(data = forest_fire, aes(x = DC, y = size_category, fill = size_category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("DC in Terms of Size of Area")

by(forest_fire$DC, forest_fire$size_category, summary)

# ISI
ggplot(data = forest_fire, aes(x = ISI)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("ISI")
summary(forest_fire$ISI)

ggplot(data = forest_fire, aes(x = ISI, y = size_category, fill = size_category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("ISI in Terms of Size of Area")

by(forest_fire$ISI, forest_fire$size_category, summary)

# Temperature
ggplot(data = forest_fire, aes(x = temp)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +2
  theme_fivethirtyeight() +
  ggtitle("Temperature")
summary(forest_fire$temp)

ggplot(data = forest_fire, aes(x = temp, y = size_category, fill = size_category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Temperature in Terms of Size of Area")

by(forest_fire$temp, forest_fire$size_category, summary)

# RH
ggplot(data = forest_fire, aes(x = RH)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("RH")
summary(forest_fire$RH)

ggplot(data = forest_fire, aes(x = RH, y = size_category, fill = size_category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("RH in Terms of Size of Area")

by(forest_fire$RH, forest_fire$size_category, summary)

# Wind
ggplot(data = forest_fire, aes(x = wind)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Wind")
summary(forest_fire$wind)

ggplot(data = forest_fire, aes(x = wind, y = size_category, fill = size_category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Wind in Terms of Size of Area")

by(forest_fire$wind, forest_fire$size_category, summary)

# Rain
ggplot(data = forest_fire, aes(x = rain)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Rain")
summary(forest_fire$rain)

ggplot(data = forest_fire, aes(x = rain, y = size_category, fill = size_category)) +
  geom_boxplot() +
  theme_fivethirtyeight() +
  ggtitle("Rain in Terms of Size of Area")

by(forest_fire$rain, forest_fire$size_category, summary)

# Area
ggplot(data = forest_fire, aes(x = area)) +
  geom_boxplot(color="black", fill="#ff2424", lwd = 0.9) +
  theme_fivethirtyeight() +
  ggtitle("Area")
summary(forest_fire$area)

##### NORMALIZING
FF_norm <- forest_fire[-c(1,2)]
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
FF_norm$RH   = normalize(FF_norm$RH)
FF_norm$wind = normalize(FF_norm$wind)
FF_norm$temp = normalize(FF_norm$temp)
FF_norm$area = normalize(FF_norm$area)
FF_norm$FFMC = normalize(FF_norm$FFMC)
FF_norm$DMC = normalize(FF_norm$DMC)
FF_norm$DC = normalize(FF_norm$DC)
FF_norm$ISI = normalize(FF_norm$ISI)

View(FF_norm)

forest_train <- FF_norm[1:413, ]
forest_test <- FF_norm[414:517, ]

##############----- SVM MODELS -----#############

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", 
# "besseldot", "anovadot", "splinedot", "laplacedot"

### 1 - rbfdot
FF_model_1 <- ksvm(size_category ~. , 
                  data = forest_train, kernel = "rbfdot")
FF_pred_1 <- predict(FF_model_1, newdata = forest_test)
mean(FF_pred_1 == forest_test$size_category) # = 0.7019231

### 2 - polydot
FF_model_2 <- ksvm(size_category ~. , 
                   data = forest_train, kernel = "polydot")
FF_pred_2 <- predict(FF_model_2, newdata = forest_test)
mean(FF_pred_2 == forest_test$size_category) # = 0.7115385

### 3 - tanhdot
FF_model_3 <- ksvm(size_category ~. , 
                   data = forest_train, kernel = "tanhdot")
FF_pred_3 <- predict(FF_model_3, newdata = forest_test)
mean(FF_pred_3 == forest_test$size_category) # = 0.7019231

### 4 - vanilladot
FF_model_4 <- ksvm(size_category ~. , 
                   data = forest_train, kernel = "vanilladot")
FF_pred_4 <- predict(FF_model_4, newdata = forest_test)
mean(FF_pred_4 == forest_test$size_category) # = 0.7115385

### 5 - besseldot
FF_model_5 <- ksvm(size_category ~. , 
                   data = forest_train, kernel = "besseldot")
FF_pred_5 <- predict(FF_model_5, newdata = forest_test)
mean(FF_pred_5 == forest_test$size_category) # = 0.7019231

### 6 - anovadot
FF_model_6 <- ksvm(size_category ~. , 
                   data = forest_train, kernel = "anovadot")
FF_pred_6 <- predict(FF_model_6, newdata = forest_test)
mean(FF_pred_6 == forest_test$size_category) # = 0.7019231

### 7 - splinedot
FF_model_7 <- ksvm(size_category ~. , 
                   data = forest_train, kernel = "splinedot")
FF_pred_7 <- predict(FF_model_7, newdata = forest_test)
mean(FF_pred_7 == forest_test$size_category) # = 0.5673077

### 8 - laplacedot
FF_model_8 <- ksvm(size_category ~. , 
                   data = forest_train, kernel = "laplacedot")
FF_pred_8 <- predict(FF_model_8, newdata = forest_test)
mean(FF_pred_8 == forest_test$size_category) # = 0.7019231

table(FF_pred_4, forest_test$size_category)
