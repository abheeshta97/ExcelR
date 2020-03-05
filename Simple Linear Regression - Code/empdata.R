#3) Emp_data -> Build a prediction model for Churn_out_rate 

library(pacman)
p_load(readr, ggplot2)

emp <- read_csv(file.choose()) #emp_data.csv
View(emp)
attach(emp)

plot(Salary_hike, Churn_out_rate, col = "blue", cex = 1.5)
cor(Salary_hike, Churn_out_rate)

emp.model <- lm(Churn_out_rate ~ Salary_hike)
emp.model

churnPred <- predict(emp.model)
churnPred

churnPredInt <- predict(emp.model, interval = "prediction")
churnPredInt

empNew <- cbind(emp, churnPredInt)
ggplot(data = empNew, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color = "blue", size = 3) +
  geom_line(color = "red", data = emp, aes(x= Salary_hike, y = churnPred), size = 2) +
  geom_line(aes(y = lwr), linetype = "dashed") +
  geom_line(aes(y = upr), linetype = "dashed")
