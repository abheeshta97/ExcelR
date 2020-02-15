library(readr)

cars <- read_csv(file.choose())

attach(cars)

M <- mean(MPG)
S <- sd(MPG)

#P(MPG > 38)
pnorm(38, M, S)
ans1 <- 1 - pnorm(38, M, S)
ans1

#P(MPG<40)
ans2 <- pnorm(40, M, S)
ans2

#P(20 < MPG < 50)
ans3 <- pnorm(50, M, S) - pnorm(20, M, S)
ans3