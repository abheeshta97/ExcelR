# HYPOTHESIS TESTING - PRADEEP ABHEESHTA
# Customer Order Form


library(car)
library(tidyverse)
library(BSDA)
library(nortest)

cust <- read.csv(file.choose())
View(cust)

table(cust$Phillippines)
table(cust$Indonesia)
table(cust$Malta)
table(cust$India)

Country <- c(colnames(cust))
Defective <- c(29, 33, 31, 20)
Error.Free <- c(271, 267, 269, 280)
cust_DF <- data.frame(row.names = Country, Defective, Error.Free)
View(cust_DF)

#####----- CHI SQUARE TEST -----#####
chisq.test(cust_DF)
# p-value = 0.2771 > 0.05 => Accept Null Hypothesis