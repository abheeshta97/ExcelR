#Basic Stats 1 - Question 7
q7 <- read.csv(file.choose())

summary(q7)

library(modeest)

#Most Frequent Value
mfv(q7$Points)
mfv(q7$Score)
mfv(q7$Weigh)

sd(q7$Points)
var(q7$Points)
range(q7$Points)

sd(q7$Score)
var(q7$Score)
range(q7$Score)

sd(q7$Weigh)
var(q7$Weigh)
range(q7$Weigh)




