library(readr)
library(ggplot2)

sal <- read_csv(file.choose()) #Salary_data.csv
View(sal)
attach(sal) 

plot(YearsExperience, Salary, cex = 1.5, col = "blue")
cor(YearsExperience, Salary)

#Linear Model
sal.model <- lm(Salary ~ YearsExperience)
summary(sal.model)
mean(sal.model$residuals^2) #MSE
sqrt(mean(sal.model$residuals^2)) #RMSE
mean(abs(sal.model$residuals)) #MAE
#Multiple R-squared:  0.957,	Adjusted R-squared:  0.9554

#log model
sal.model.log <- lm(Salary ~ log(YearsExperience))
summary(sal.model.log)
mean(sal.model.log$residuals^2) #MSE
sqrt(mean(sal.model.log$residuals^2)) #RMSE
mean(abs(sal.model.log$residuals)) #MAE
#Multiple R-squared:  0.8539,	Adjusted R-squared:  0.8487 

#Exp Model
sal.model.exp <- lm(log(Salary) ~ YearsExperience)
summary(sal.model.exp)
mean(sal.model.exp$residuals^2) #MSE
sqrt(mean(sal.model.exp$residuals^2)) #RMSE
mean(abs(sal.model.exp$residuals)) #MAE
#Multiple R-squared:  0.932,	Adjusted R-squared:  0.9295 

#Polynomial 2 Degree Model
sal.model.poly <- lm(log(Salary) ~ YearsExperience + I(YearsExperience^2))
summary(sal.model.poly)
mean(sal.model.poly$residuals^2) #MSE
sqrt(mean(sal.model.poly$residuals^2)) #RMSE
mean(abs(sal.model.poly$residuals)) #MAE
#Multiple R-squared:  0.9486,	Adjusted R-squared:  0.9448

#Predictions - Polynomial 2 Degree
salPred <- predict(sal.model.poly)
salPred

salPredInt <- predict(sal.model.poly, interval = "prediction")
salPredInt

salPredConf <- predict(sal.model.poly, interval = "confidence")
salPredConf

#plot
ggplot(data = sal, aes(x = YearsExperience + I(YearsExperience^2), y= log(Salary))) +
  geom_point(color = "blue", size = 3) +
  geom_line(size = 2, color = "red", data = sal, aes(x = YearsExperience + I(YearsExperience^2), y = salPred))
