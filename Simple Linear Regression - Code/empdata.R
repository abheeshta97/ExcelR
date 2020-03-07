#3) Emp_data -> Build a prediction model for Churn_out_rate 

library(pacman)
p_load(readr, ggplot2)

emp <- read_csv(file.choose()) #emp_data.csv
View(emp)
attach(emp)

plot(Salary_hike, Churn_out_rate, col = "blue", cex = 1.5)
cor(Salary_hike, Churn_out_rate)

#Linear Model
emp.model <- lm(Churn_out_rate ~ Salary_hike)
summary(emp.model)
mean(emp.model$residuals^2) #MSE
sqrt(mean(emp.model$residuals^2)) #RMSE
mean(abs(emp.model$residuals)) #MAE
#Multiple R-squared:  0.8312,	Adjusted R-squared:  0.8101 

#Log Model
emp.model.log <- lm(Churn_out_rate ~ log(Salary_hike))
summary(emp.model.log)
mean(emp.model.log$residuals^2) #MSE
sqrt(mean(emp.model.log$residuals^2)) #RMSE
mean(abs(emp.model.log$residuals)) #MAE
#Multiple R-squared:  0.8486,	Adjusted R-squared:  0.8297 

#Exp Model
emp.model.exp <- lm(log(Churn_out_rate) ~ Salary_hike)
summary(emp.model.exp)
mean(emp.model.exp$residuals^2) #MSE
sqrt(mean(emp.model.exp$residuals^2)) #RMSE
mean(abs(emp.model.exp$residuals)) #MAE
#Multiple R-squared:  0.8735,	Adjusted R-squared:  0.8577 

#Polynomial 2 Degree Model
emp.model.poly <- lm(log(Churn_out_rate) ~ Salary_hike + I(Salary_hike^2))
summary(emp.model.poly)
mean(emp.model.poly$residuals^2) #MSE
sqrt(mean(emp.model.poly$residuals^2)) #RMSE
mean(abs(emp.model.poly$residuals)) #MAE
#Multiple R-squared:  0.9836,	Adjusted R-squared:  0.9789 

#Predictions
churnPred <- predict(emp.model.poly)
churnPred

churnPredInt <- predict(emp.model.poly, interval = "prediction")
churnPredInt

churnPredConf <- predict(emp.model.poly, interval = "confidence")
churnPredConf

#Plot
ggplot(data = emp, aes(x = Salary_hike + I(Salary_hike^2), y= log(Churn_out_rate))) +
  geom_point(color = "blue", size = 3) +
  geom_line(size = 2, color = "red", data = emp, aes(x= Salary_hike + I(Salary_hike^2), y = churnPred))
