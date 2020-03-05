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
cal.model <- lm(`Weight gained (grams)` ~ `Calories Consumed`)
cal.model
cal.model$residuals

weightPred <- predict(cal.model)
weightPred

weightPredInt <- predict(cal.model, interval = "prediction")
weightPredInt

calNew <- cbind(cal, weightPredInt)
ggplot(data = calNew, aes(x = `Calories Consumed`, y = `Weight gained (grams)`)) + 
  geom_point(color = "blue", size = 3) +
  geom_line(color = "red", data = cal, aes(x= `Calories Consumed`, y = weightPred), size = 2) +
  geom_line(aes(y = lwr), linetype = "dashed") +
  geom_line(aes(y = upr), linetype = "dashed")
