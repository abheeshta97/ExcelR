library(readr)
library(ggplot2)

sal <- read_csv(file.choose()) #Salary_data.csv
View(sal)
attach(sal) 

plot(YearsExperience, Salary, cex = 1.5, col = "blue")
cor(YearsExperience, Salary)

sal.model <- lm(Salary ~ YearsExperience)
sal.model

salPred <- predict(sal.model)
salPred

salPredInt <- predict(sal.model, interval = "prediction")
salPredInt

salNew <- cbind(sal, salPredInt)
ggplot(data = salNew, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color = "blue", size = 3) +
  geom_line(color = "red", data = sal, aes(x= YearsExperience, y = salPred), size = 2) +
  geom_line(aes(y = lwr), linetype = "dashed") +
  geom_line(aes(y = upr), linetype = "dashed")
