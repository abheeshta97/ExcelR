# HYPOTHESIS TESTING - PRADEEP ABHEESHTA
# Buyer Ratio

library(car)
library(ggthemes)
library(tidyverse)
library(BSDA)
library(nortest)
library(reshape2)
library(MASS)

buyer <- read.csv(file.choose(), row.names = 1) #BuyerRatio.csv
View(buyer)
str(buyer)

chisq.test(buyer)
# p-value = 0.6603 > 0.05 => Accept Null Hypothesis