# Forecasting Airlines Data - Airlines+Data.xslx
# Pradeep Abheeshta

library(fpp)
library(ggplot2)
library(ggthemes)
library(Metrics)
library(readxl)

library(tseries)

airlines <- read_excel(file.choose())
View(airlines)
str(airlines)
plot(airlines$Passengers, type = "o")

## DUMMY VARIABLES
dummy_airlines <- data.frame(outer(rep(month.abb, length = 96), month.abb, "==") + 0 )
View(dummy_airlines)
colnames(dummy_airlines) <- month.abb 
AirlinesData <- cbind(airlines, dummy_airlines)
View(AirlinesData)

AirlinesData["t"]<- 1:96
View(AirlinesData)
AirlinesData["log_Passengers"]<-log(AirlinesData["Passengers"])
AirlinesData["t_square"]<-AirlinesData["t"]*AirlinesData["t"]
airlines_train <- AirlinesData[1:84,]
airlines_test <- AirlinesData[85:96,]

## LINEAR MODEL
air_linear_model <- lm(Passengers~t, data = airlines_train)
summary(air_linear_model)
# Multiple R-squared:  0.7923,	Adjusted R-squared:  0.7898
air_linear_pred <- data.frame(predict(air_linear_model, interval='predict', newdata = airlines_test))
air_linear_rmse <- rmse(airlines_test$Passengers, air_linear_pred$fit)
air_linear_rmse # = 53.19924

## EXPONENTIAL MODEL
air_exp_model <- lm(log_Passengers~t, data = airlines_train)
summary(air_exp_model)
# Multiple R-squared:  0.8239,	Adjusted R-squared:  0.8218 
air_exp_pred <- data.frame(predict(air_exp_model, interval='predict', newdata = airlines_test))
air_exp_rmse <- rmse(airlines_test$Passengers, air_exp_pred$fit)
air_exp_rmse # = 325.7151

## QUADRATIC MODEL
air_quad_model <- lm(log_Passengers~t+t_square, data = airlines_train)
summary(air_quad_model)
# Multiple R-squared:  0.8258,	Adjusted R-squared:  0.8215  
air_quad_pred <- data.frame(predict(air_quad_model, interval='predict', newdata = airlines_test))
air_quad_rmse <- rmse(airlines_test$Passengers, air_quad_pred$fit)
air_quad_rmse # = 325.7567

## ADDITIVE SEASONALITY
air_add_sea_model <- lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = airlines_train)
summary(air_add_sea_model)
# Multiple R-squared:  0.1674,	Adjusted R-squared:  0.04015   
air_add_sea_pred <- data.frame(predict(air_add_sea_model, interval='predict', newdata = airlines_test))
air_add_sea_rmse <- rmse(airlines_test$Passengers, air_add_sea_pred$fit)
air_add_sea_rmse # = 132.8198

## ADDITIVE SEASONALITY (LINEAR)
air_add_sea_linear_model <- lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = airlines_train)
summary(air_add_sea_linear_model)
# Multiple R-squared:  0.9551,	Adjusted R-squared:  0.9475 
air_add_sea_linear_pred <- data.frame(predict(air_add_sea_linear_model, interval='predict', newdata = airlines_test))
air_add_sea_linear_rmse <- rmse(airlines_test$Passengers, air_add_sea_linear_pred$fit)
air_add_sea_linear_rmse # = 35.34896

## ADDITIVE SEASONALITY (QUADRATIC)
air_add_sea_quad_model <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = airlines_train)
summary(air_add_sea_quad_model)
# Multiple R-squared:  0.9598,	Adjusted R-squared:  0.9524 
air_add_sea_quad_pred <- data.frame(predict(air_add_sea_quad_model, interval='predict', newdata = airlines_test))
air_add_sea_quad_rmse <- rmse(airlines_test$Passengers, air_add_sea_quad_pred$fit)
air_add_sea_quad_rmse # = 26.36082

## MULTIPLICATIVE SEASONALITY
air_mult_sea_model <- lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = airlines_train)
summary(air_mult_sea_model)
# Multiple R-squared:  0.1548,	Adjusted R-squared:  0.02568   
air_mult_sea_pred <- data.frame(predict(air_mult_sea_model, interval='predict', newdata = airlines_test))
air_mult_sea_rmse <- rmse(airlines_test$Passengers, air_mult_sea_pred$fit)
air_mult_sea_rmse # = 326.2268

## MULTIPLICATIVE SEASONALITY (LINEAR)
air_mult_sea_linear_model <- lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = airlines_train)
summary(air_mult_sea_linear_model)
# Multiple R-squared:  0.9763,	Adjusted R-squared:  0.9723  
air_mult_sea_linear_pred <- data.frame(predict(air_mult_sea_linear_model, interval='predict', newdata = airlines_test))
air_mult_sea_linear_rmse <- rmse(airlines_test$Passengers, air_mult_sea_linear_pred$fit)
air_mult_sea_linear_rmse # =  325.6952

## FINAL MODEL - ADDITIVE SEASONALITY (QUAD)

air_new_model <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = AirlinesData)
air_new_pred <- data.frame(predict(air_new_model, newdata = AirlinesData, interval='predict'))

air_new_model_fin <- air_new_model$fitted.values

View(air_new_model_fin)

air_pred_res <- predict(arima(AirlinesData$Passengers, order=c(1,0,0)),n.ahead = 12)
Month <- as.data.frame(airlines$Month)

air_final <- as.data.frame(cbind(Month,AirlinesData$Passengers, air_new_model_fin))
colnames(air_final) <-c("Month","Passengers","New_Pred_Value")
air_final <- as.data.frame(air_final)
View(air_final)

plot(air_final$Passengers, main = "Actual Graph [Airlines]", ylab = "Passengers - Actual", xlab = "Months", type = "o") 
plot(air_final$New_Pred_Value, main = "Predicted Graph [Airlines]", ylab = "Passengers - Predicted", xlab= "Months", type = "s")
