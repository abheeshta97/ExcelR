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

reg <- lm(`Delivery Time` ~ `Sorting Time`)
reg

summary(reg)

deliveryPred <- predict(reg)
deliveryPred

reg$residuals
sqrt(sum(reg$residuals^2)/nrow(delivery)) #RMSE

confint(reg, level = 0.95)
pred_int <- predict(reg, interval = "prediction")
pred_int
predict(reg, interval = "confidence")

delivery_new <- cbind(delivery, pred_int)
ggplot(data = delivery_new, aes(x = `Sorting Time`, y = `Delivery Time`)) + 
  geom_point(color = "blue", size = 3) +
  geom_line(color = "red", data = delivery, aes(x= `Sorting Time`, y = deliveryPred), size = 2) +
  geom_line(aes(y = lwr), linetype = "dashed") +
  geom_line(aes(y = upr), linetype = "dashed")
