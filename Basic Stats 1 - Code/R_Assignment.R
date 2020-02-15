#Basic Stats - 1
#Dataset Used - mba.csv

library(readr)


mba <- read_csv(file.choose())

summary(mba)

boxplot(mba$workex, horizontal = TRUE, col = "cyan", main = "Boxplot - Work Ex")
boxplot(mba$gmat, horizontal = TRUE, col = "pink", main = "Boxplot - GMAT")

hist(mba$workex, col = "cyan", main = "Histogram - Work Ex", xlab = "WorkEx", freq = FALSE)
curve(dnorm(x, mean=mean(mba$workex), sd=sd(mba$workex)), add=TRUE, col="blue")

hist(mba$gmat, col = "pink", main = "Histogram - GMAT", xlab = "GMAT", freq = FALSE)
curve(dnorm(x, mean=mean(mba$gmat), sd=sd(mba$gmat)), add=TRUE, col="red")



