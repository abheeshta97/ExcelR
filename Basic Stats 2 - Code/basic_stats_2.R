#Set 1 - Question 1
banks <- c(0.2423,
           0.2553,
           0.2541,
           0.2414,
           0.2962,
           0.2825,
           0.2581,
           0.2439,
           0.4026,
           0.3295,
           0.9136,
           0.2599,
           0.3942,
           0.2671,
           0.3500
)

mean(banks)
var(banks)
sd(banks)

#Set 2 - Question 1
1 - pnorm(50, 45, 8)

#Set 2 - Question 5
div_1 <- pnorm(0,5, 3)
div_1
div_2 <- pnorm(0, 7, 4)
div_2

#Set 4 - Question 3
1 - (pnorm(55, 50, 4) - pnorm(45,50,4))

#Set 4 - Question 4
qnorm(0.975)
