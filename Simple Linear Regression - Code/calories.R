#Question 1: Calories_Consumed-> 
#Predict Weight Gained Using Calories Consumed

library(pacman)
p_load(readr, ggplot2)

cal <- read_csv(file.choose()) #calories_consumed.csv
View(cal)
summary(cal)
attach(cal)

plot(`Calories Consumed`, `Weight gained (grams)`, cex = 1.5, col = "blue")
cor(`Calories Consumed`, `Weight gained (grams)`)

#Linear Model
cal.model <- lm(`Weight gained (grams)` ~ `Calories Consumed`)
summary(cal.model)
mean(cal.model$residuals^2) #MSE
sqrt(mean(cal.model$residuals^2)) #RMSE
mean(abs(cal.model$residuals)) #MAE
#> mean(cal.model$residuals^2) #MSE
#[1] 10671.41
#> sqrt(mean(cal.model$residuals^2)) #RMSE
#[1] 103.3025
#> mean(abs(cal.model$residuals)) #MAE
#[1] 93.65721


#Logarithmic Model
#x = log(Calories); y = Weight Gained
plot(log(`Calories Consumed`), `Weight gained (grams)`)
cal.model.log <- lm(`Weight gained (grams)` ~ log(`Calories Consumed`))
summary(cal.model.log)
mean(cal.model.log$residuals^2) #MSE
sqrt(mean(cal.model.log$residuals^2)) #RMSE
mean(abs(cal.model.log$residuals)) #MAE
# > mean(cal.model.log$residuals^2) #MSE
# [1] 19882.52
# > sqrt(mean(cal.model.log$residuals^2)) #RMSE
# [1] 141.0054
# > mean(abs(cal.model.log$residuals)) #MAE
# [1] 125.2663

#Exponential Model
plot(`Calories Consumed`, log(`Weight gained (grams)`))
cal.model.exp <- lm(log(`Weight gained (grams)`) ~ `Calories Consumed`)
summary(cal.model.exp)
mean(cal.model.exp$residuals^2) #MSE
sqrt(mean(cal.model.exp$residuals^2)) #RMSE
mean(abs(cal.model.exp$residuals)) #MAE
#> mean(cal.model.exp$residuals^2) #MSE
#[1] 0.09414024
#> sqrt(mean(cal.model.exp$residuals^2)) #RMSE
#[1] 0.3068228
#> mean(abs(cal.model.exp$residuals)) #MAE
#[1] 0.2102816

#Polynomial Model
cal.model.poly <- lm(log(`Weight gained (grams)`) ~ 
                       `Calories Consumed` + I(`Calories Consumed`^2))
summary(cal.model.poly)
mean(cal.model.poly$residuals^2) #MSE
sqrt(mean(cal.model.poly$residuals^2)) #RMSE
mean(abs(cal.model.poly$residuals)) #MAE
# > mean(cal.model.poly$residuals^2) #MSE
# [1] 0.09413942
# > sqrt(mean(cal.model.poly$residuals^2)) #RMSE
# [1] 0.3068215
# > mean(abs(cal.model.poly$residuals)) #MAE
# [1] 0.210327

#Best Model - Exp Model

weightPred <- predict(cal.model.exp)
weightPred

weightPredInt <- predict(cal.model.exp, interval = "prediction")
weightPredInt

weightPredConf <- predict(cal.model.exp, interval = "confidence")
weightPredConf

#Plot
ggplot(data = cal, aes(x = `Calories Consumed`, y= log(`Weight gained (grams)`))) +
  geom_point(color = "blue", size = 3) +
  geom_line(size = 2, color = "red", data = cal, aes(x=`Calories Consumed`, y = weightPred))
