# Forecasting Platsic Sales - PlasticSales.csv
# Pradeep Abheeshta

library(fpp)
library(ggplot2)
library(ggthemes)
library(Metrics)

plastic <- read.csv(file.choose())
View(plastic)
str(plastic)

plot(plastic$Sales, type = 'o')

## DUMMY VARIABLES

dummy_var <- data.frame(outer(rep(month.abb,length = 60), month.abb ,"==") + 0 ) 
View(dummy_var)

colnames(dummy_var) <- month.abb # Assigning month names 
View(dummy_var)
plastic_1 <- cbind(plastic, dummy_var)

View(plastic_1)

plastic_1["t"] <- 1:60
View(plastic_1)
plastic_1["log_Sales"] <- log(plastic_1["Sales"])
plastic_1["t_square"]<-plastic_1["t"] * plastic_1["t"]

plastic_train <- plastic_1[1:48, ]
plastic_test <- plastic_1[49:60, ]

## LINEAR MODEL
plastic_linear_model <- lm(data = plastic_train, Sales ~ t)
summary(plastic_linear_model)
# Multiple R-squared:  0.3305,	Adjusted R-squared:  0.3159 
plastic_linear_pred <- data.frame(predict(plastic_linear_model, interval='predict', newdata = plastic_test))
plastic_linear_rmse <- rmse(plastic_test$Sales, plastic_linear_pred$fit)
plastic_linear_rmse # = 260.9378

## EXP MODEL
plastic_exp_model <- lm(data = plastic_train, log_Sales~t)
summary(plastic_exp_model)
# Multiple R-squared:  0.3173,	Adjusted R-squared:  0.3025 
plastic_exp_pred <- data.frame(predict(plastic_exp_model, interval = 'predict', newdata = plastic_test))
plastic_exp_rmse <- rmse(plastic_test$Sales, plastic_exp_pred$fit)
plastic_exp_rmse # = 1337.275

## QUADRATIC MODEL

plastic_quad_model <- lm(data = plastic_train, Sales ~ t+t_square)
summary(plastic_quad_model)
# Multiple R-squared:  0.3344,	Adjusted R-squared:  0.3048 
plastic_quad_pred <- data.frame(predict(plastic_quad_model, interval = 'predict', newdata = plastic_test))
plastic_quad_rmse <- rmse(plastic_test$Sales, plastic_quad_pred$fit)
plastic_quad_rmse # = 297.4067

## ADDITIVE SEASONALITY

plastic_add_sea_model <- lm(data = plastic_train, Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec)
summary(plastic_add_sea_model)
# Multiple R-squared:  0.7691,	Adjusted R-squared:  0.6985 
plastic_add_sea_pred <- data.frame(predict(plastic_add_sea_model, interval = 'predict', newdata = plastic_test))
plastic_add_sea_rmse <- rmse(plastic_test$Sales, plastic_add_sea_pred$fit)
plastic_add_sea_rmse # = 235.6027

## ADDITIVE SEASONALITY WITH LINEAR MODEL

plastic_add_sea_linear_model <- lm(data = plastic_train, Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec)
summary(plastic_add_sea_linear_model)
# Multiple R-squared:  0.9736,	Adjusted R-squared:  0.9645 
plastic_add_sea_linear_pred <- data.frame(predict(plastic_add_sea_linear_model, interval = 'predict', newdata = plastic_test))
plastic_add_sea_linear_rmse <- rmse(plastic_test$Sales, plastic_add_sea_linear_pred$fit)
plastic_add_sea_linear_rmse # = 135.5536

## ADDITIVE SEASONALITY WITH QUADRATIC

plastic_add_sea_quad_model <- lm(data = plastic_train, Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec)
summary(plastic_add_sea_quad_model) 
# Multiple R-squared:  0.9832,	Adjusted R-squared:  0.9768 
plastic_add_sea_quad_pred <- data.frame(predict(plastic_add_sea_quad_model, interval = 'predict', newdata = plastic_test))
plastic_add_sea_quad_rmse <- rmse(plastic_test$Sales, plastic_add_sea_quad_pred$fit)
plastic_add_sea_quad_rmse # = 218.1939

## MULTIPLICATIVE SEASONALITY 

plastic_multi_sea_model <- lm(data = plastic_train, log_Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec)
summary(plastic_multi_sea_model)
# Multiple R-squared:  0.7916,	Adjusted R-squared:  0.728 
plastic_multi_sea_pred <- data.frame(predict(plastic_multi_sea_model, interval = 'predict', newdata = plastic_test))
plastic_multi_sea_rmse <- rmse(plastic_test$Sales, plastic_multi_sea_pred$fit)
plastic_multi_sea_rmse # = 1337.526

## MULTIPLICATIVE SEASONALITY LINEAR

plastic_multi_sea_linear_model <- lm(data = plastic_train, log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec)
summary(plastic_multi_sea_linear_model)
# Multiple R-squared:  0.9815,	Adjusted R-squared:  0.9751 
plastic_multi_sea_linear_pred <- data.frame(predict(plastic_multi_sea_linear_model, interval = 'predict', newdata = plastic_test))
plastic_multi_sea_linear_rmse <- rmse(plastic_test$Sales, plastic_multi_sea_linear_pred$fit)
plastic_multi_sea_linear_rmse # 1337.301

## FINAL MODEL - ADDITIVE SEASONALITY WITH LINEAR MODEL

plastic_new_model <- lm(data = plastic_1, Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec)
plastic_new_pred <- data.frame(predict(plastic_new_model, newdata = plastic_1, interval='predict'))

plastic_new_model_fin <- plastic_new_model$fitted.values

View(plastic_new_model_fin)

Month <- as.data.frame(plastic_1$Month)

final <- as.data.frame(cbind(Month, plastic_1$Sales, plastic_new_model_fin))
colnames(final) <-c("Month","Sales","New_Pred_Value")

plot(final$Sales,main = "Actual Graph [Plastic]", xlab = "Months", ylab= "Sales - Actual", type="o") 
plot(final$New_Pred_Value, main = "Predicted Graph [Plastic]", xlab = "Months", ylab="Sales - Predicted", type="s")

