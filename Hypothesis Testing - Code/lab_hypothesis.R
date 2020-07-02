# HYPOTHESIS TESTING - PRADEEP ABHEESHTA
#labTAT.csv

library(car)
library(ggthemes)
library(tidyverse)
library(BSDA)
library(nortest)

lab <- read.csv(file.choose()) #labTAT.csv
View(lab)

str(lab)
anyNA(lab)
lab_stacked <- stack(lab)
View(lab_stacked)

ggplot(data = lab_stacked, aes(x = values, y = ind, fill = ind)) +
  theme_fivethirtyeight()  +
  geom_boxplot(color="black", lwd = 0.7) +
  ggtitle("Labs")
summary(lab)
#####----- NORMALITY TEST -----#####
shapiro.test(lab$Laboratory.1)
# 0.5508 > 0.05 = normal
shapiro.test(lab$Laboratory.2)
# 0.8637 > 0.05 = normal
shapiro.test(lab$Laboratory.3)
# 0.4205 > 0.05 = normal
shapiro.test(lab$Laboratory.4)
# 0.6619 > 0.05 = normal
# CONCLUSION: ALL OF THEM ARE NORMAL

#####---- CHECK FOR EQUAL VARIANCES ----#####
bartlett.test(values ~ ind, data = lab_stacked)
# CONCLUSION: p-value (0.1069) > 0.05 => Cannot reject null hypothesis

#####---- ONE WAY ANOVA -----#####
lab_aov_res <- aov(values ~ ind, data = lab_stacked)
summary(lab_aov_res)
# CONCLUSION: p-value (< 2e-16) < 0.05 => Reject null hypothesis 
# Significant differences between the groups