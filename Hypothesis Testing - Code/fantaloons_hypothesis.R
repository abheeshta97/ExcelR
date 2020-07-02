# HYPOTHESIS TESTING - PRADEEP ABHEESHTA
# Fantaloons


library(car)
library(ggthemes)
library(tidyverse)
library(BSDA)
library(nortest)

fanta <- read.csv(file.choose()) #Fantaloons.csv
View(fanta)
str(fanta)

table(fanta$Weekdays)
table(fanta$Weekend)

#####----- 2 PROP TEST -----#####
prop.test(x=c(113,167),n=c(400, 400), correct = FALSE)
# p-value = 6.256e-05 < 0.05 => Reject Null Hypothesis

