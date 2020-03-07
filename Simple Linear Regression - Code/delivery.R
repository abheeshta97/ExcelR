#2) Delivery_time -> Predict delivery 
#time using sorting time 

library(pacman)
p_load(readr, ggplot2)
delivery <- read_csv(file.choose()) #delivery_time.csv
View(delivery)
summary(delivery)
attach(delivery)

plot(`Sorting Time`,`Delivery Time`, col = "blue", cex = 1.5)
cor(`Sorting Time`, `Delivery Time`)

#Linear Model
reg <- lm(`Delivery Time` ~ `Sorting Time`)
summary(reg)
mean(reg$residuals^2) #MSE
sqrt(mean(reg$residuals^2)) #RMSE
mean(abs(reg$residuals)) #MAE
#Multiple R-squared:  0.6823,	Adjusted R-squared:  0.6655 

#Logarithmic Model
del.model.log <- lm(`Delivery Time` ~ log(`Sorting Time`))
summary(del.model.log)
mean(del.model.log$residuals^2) #MSE
sqrt(mean(del.model.log$residuals^2)) #RMSE
mean(abs(del.model.log$residuals)) #MAE
#Multiple R-squared:  0.6954,	Adjusted R-squared:  0.6794 

#Exponential Model
del.model.exp <- lm(log(`Delivery Time`) ~ `Sorting Time`)
summary(del.model.exp)
mean(del.model.exp$residuals^2) #MSE
sqrt(mean(del.model.exp$residuals^2)) #RMSE
mean(abs(del.model.exp$residuals)) #MAE
#Multiple R-squared:  0.7109,	Adjusted R-squared:  0.6957

#Polynomial 2 Degree
del.model.poly <- lm(log(`Delivery Time`) ~ `Sorting Time` + I(`Sorting Time`^2))
summary(del.model.poly)
mean(del.model.poly$residuals^2) #MSE
sqrt(mean(del.model.poly$residuals^2)) #RMSE
mean(abs(del.model.poly$residuals)) #MAE
#Multiple R-squared:  0.7649,	Adjusted R-squared:  0.7387
# > mean(del.model.poly$residuals^2) #MSE
# [1] 0.02267657
# > sqrt(mean(del.model.poly$residuals^2)) #RMSE
# [1] 0.1505874
# > mean(abs(del.model.poly$residuals)) #MAE
# [1] 0.1231183

#Predictions using polynomial 2 degree model
deliveryPred <- predict(del.model.poly)
deliveryPred

deliveryPredInt <- predict(del.model.poly, interval = "prediction")
deliveryPredInt

deliveryPredConf <- predict(del.model.poly, interval = "confidence")
deliveryPredConf

#Plot
ggplot(data = delivery, aes(x = `Sorting Time` + I(`Sorting Time`^2), y= log(`Delivery Time`))) +
  geom_point(color = "blue", size = 3) +
  geom_line(size = 2, color = "red", data = delivery, aes(x=`Sorting Time` + I(`Sorting Time`^2), y = deliveryPred))
