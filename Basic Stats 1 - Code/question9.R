library(readr)
library(moments)
#Dataset used 
q9a <- read_csv(file.choose())

attach(q9a)

skewness(speed)
kurtosis(speed)
hist(speed)

skewness(dist)
kurtosis(dist)
hist(dist)

q9b <- read_csv(file.choose())

attach(q9b)

skewness(SP)
kurtosis(SP)
hist(SP)

skewness(WT)
kurtosis(WT)
hist(WT)
