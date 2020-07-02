# HYPOTHESIS TESTING - PRADEEP ABHEESHTA
# Cutlets


library(car)
library(ggthemes)
library(tidyverse)
library(BSDA)
library(nortest)


cutlets <- read.csv(file.choose()) #Cutlets.csv
View(cutlets)

str(cutlets)
anyNA(cutlets)

cutlets_stacked <- stack(cutlets)
View(cutlets_stacked)

ggplot(data = cutlets_stacked, aes(x = values, y = ind, fill = ind)) +
  theme_fivethirtyeight()  +
  geom_boxplot(color="black", lwd = 0.7) +
  ggtitle("Cutlets")
summary(cutlets)

shapiro.test(cutlets$Unit.A)
shapiro.test(cutlets$Unit.B)

#####----- BARTLETT TEST -----#####
bartlett.test(values ~ ind, data = cutlets_stacked)
# p-value = 0.3136 > 0.05 => Accept Null Hypothesis

#####----- 2 SAMPLE T-TEST -----#####
t.test(data = cutlets_stacked, values ~ ind, alternative = "t",
       var.equal = TRUE)
# p-value = 0.4722 > 0.05 => Accept Null Hypothesis