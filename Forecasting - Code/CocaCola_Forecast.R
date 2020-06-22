# Forecasting Coca-Cola Sales - CocaCola_Sales_Rawdata.xlsx
#Pradeep Abheeshta

library(fpp)
library(ggplot2)
library(ggthemes)
library(Metrics)
library(readxl)

coke <- read_excel(file.choose())
str(coke)
View(coke)

plot(coke$Sales,type="o")

Q1 <-  ifelse(grepl("Q1", coke$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2", coke$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3", coke$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4", coke$Quarter),'1','0')

## DUMMY VARIABLES
coke_DF <- cbind(coke, Q1, Q2, Q3, Q4)
View(coke_DF)
colnames(coke_DF)

coke_DF["t"]<- 1:42
View(coke_DF)
coke_DF["log_Sales"] <- log(coke_DF["Sales"])
coke_DF["t_square"] <- coke_DF["t"] * coke_DF["t"]

coke_train <- coke_DF[1:33, ]
coke_test <- coke_DF[34:42, ]

## LINEAR MODEL
coke_linear_model <- lm(Sales ~ t, data = coke_train)
summary(coke_linear_model)
# Multiple R-squared:  0.7599,	Adjusted R-squared:  0.7521 
coke_linear_pred <- data.frame(predict(coke_linear_model, interval='predict', newdata = coke_test))
coke_linear_rmse <- rmse(coke_test$Sales, coke_linear_pred$fit)
coke_linear_rmse # = 811.0636

## EXP MODEL
coke_exp_model <- lm(log_Sales ~ t, data = coke_train)
summary(coke_exp_model)
# Multiple R-squared:  0.755,	Adjusted R-squared:  0.7471  
coke_exp_pred <- data.frame(predict(coke_exp_model, interval='predict', newdata = coke_test))
coke_exp_rmse <- rmse(coke_test$Sales, coke_exp_pred$fit)
coke_exp_rmse # = 4488.997

## QUAD MODEL
coke_quad_model <- lm(Sales ~ t + t_square, data = coke_train)
summary(coke_quad_model)
# Multiple R-squared:  0.8102,	Adjusted R-squared:  0.7975 
coke_quad_pred <- data.frame(predict(coke_quad_model, interval='predict', newdata = coke_test))
coke_quad_rmse <- rmse(coke_test$Sales, coke_quad_pred$fit)
coke_quad_rmse # = 423.1006

## ADD SEASONALITY
coke_add_sea_model <- lm(Sales ~ Q1 + Q2 + Q3 + Q4, data = coke_train)
summary(coke_add_sea_model)
# Multiple R-squared:  0.09353,	Adjusted R-squared:  -0.0002387 
coke_add_sea_pred <- data.frame(predict(coke_add_sea_model, interval='predict', newdata = coke_test))
coke_add_sea_rmse <- rmse(coke_test$Sales, coke_add_sea_pred$fit)
coke_add_sea_rmse # = 1884.97

## ADD SEASONALITY - LINEAR
coke_add_sea_linear_model <- lm(Sales ~ t + Q1 + Q2 + Q3 + Q4, data = coke_train)
summary(coke_add_sea_linear_model)
# Multiple R-squared:  0.8742,	Adjusted R-squared:  0.8562 
coke_add_sea_linear_pred <- data.frame(predict(coke_add_sea_linear_model, interval='predict', newdata = coke_test))
coke_add_sea_linear_rmse <- rmse(coke_test$Sales, coke_add_sea_linear_pred$fit)
coke_add_sea_linear_rmse # = 688.7017

## ADD SEASONALITY - QUAD
coke_add_sea_quad_model <- lm(Sales ~ t + t_square + Q1 + Q2 + Q3 + Q4, data = coke_train)
summary(coke_add_sea_quad_model)
# Multiple R-squared:  0.9414,	Adjusted R-squared:  0.9306 
coke_add_sea_quad_pred <- data.frame(predict(coke_add_sea_quad_model, interval='predict', newdata = coke_test))
coke_add_sea_quad_rmse <- rmse(coke_test$Sales, coke_add_sea_quad_pred$fit)
coke_add_sea_quad_rmse # = 225.9438

## MULTIPLICATIVE SEASONALITY
coke_mult_sea_model <- lm(log_Sales ~ Q1 + Q2 + Q3 + Q4, data = coke_train)
summary(coke_mult_sea_model)
#Multiple R-squared:  0.1076,	Adjusted R-squared:  0.01528 
coke_mult_sea_pred <- data.frame(predict(coke_mult_sea_model, interval='predict', newdata = coke_test))
coke_mult_sea_rmse <- rmse(coke_test$Sales, coke_mult_sea_pred$fit)
coke_mult_sea_rmse # = 4489.431

## MULTIPLICATIVE SEASONALITY - LINEAR
coke_mult_sea_linear_model <- lm(log_Sales ~ t + Q1 + Q2 + Q3 + Q4, data = coke_train)
summary(coke_mult_sea_linear_model)
# Multiple R-squared:  0.8823,	Adjusted R-squared:  0.8654 
coke_mult_sea_linear_pred <- data.frame(predict(coke_mult_sea_linear_model, interval='predict', newdata = coke_test))
coke_mult_sea_linear_rmse <- rmse(coke_test$Sales, coke_mult_sea_linear_pred$fit)
coke_mult_sea_linear_rmse # = 4488.97

## FINAL MODEL - ADD. SEASONALITY - QUADRATIC
coke_new_model <- lm(Sales ~ t + t_square + Q1 + Q2 + Q3 + Q4, data = coke_DF)
coke_new_model_pred <- data.frame(predict(coke_new_model, newdata = coke_DF, interval='predict'))

coke_new_model_fin <- coke_new_model$fitted.values

View(coke_new_model_fin)
Quarter <- as.data.frame(coke_DF$Quarter)

coke_final <- as.data.frame(cbind(Quarter, coke_DF$Sales, coke_new_model_fin))
colnames(coke_final) <- c("Quarter","Sales","New_Pred_Value")

plot(coke_final$Sales,main = "Actual Graph [Coca-Cola]", xlab = "Quarter", ylab= "Sales - Actual", type="o") 
plot(coke_final$New_Pred_Value, main = "Predicted Graph [Coca-Cola]", xlab = "Quarter", ylab="Sales - Predicted", type="s")
