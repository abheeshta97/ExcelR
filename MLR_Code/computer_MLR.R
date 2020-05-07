library(corrplot)
library(RColorBrewer)
library(sjstats)
library(car)
library(ggiraphExtra)
library(moonBook)
library(plyr)
library(ggplot2)
library(fastDummies)

comp.original <- read.csv(file.choose()) #Computer_Data.csv
View(comp.original)

comp <- dummy_cols(comp.original, select_columns = c('cd', 'multi', 'premium'), 
                   remove_first_dummy = T, remove_selected_columns = T)
comp <- comp[-c(1)]

head(comp)
View(comp)
comp.cor <- cor(comp)
corrplot(comp.cor, method = "number", col=brewer.pal(n=8, name="Dark2"))

comp.model <- lm(data = comp, price ~ .)
summary(comp.model)
sigma(comp.model)/mean(comp$price)

comp.model.1 <- lm(data = comp, price ~ . -cd_yes)
summary(comp.model.1)
sigma(comp.model.1)/mean(comp$price)

comp.model.2 <- lm(data = comp, price ~ ram + speed + screen + hd)
summary(comp.model.2)
sigma(comp.model.2)/mean(comp$price)

comp.model.3 <- lm(data = comp, price ~ . -multi_yes -premium_yes -ads)
summary(comp.model.3)
sigma(comp.model.3)/mean(comp$price)

par(mfrow = c(2,2))
plot(comp.model)
par(mfrow = c(1,1))

influenceIndexPlot(comp.model, id.n = 6)
influencePlot(comp.model, id.n = 6)

summary(lm(price ~., data = comp[-c(1441, 1701, 3784, 4478 ), ]))
comp.model.final <- lm(price ~., data = comp[-c(1441, 1701, 3784, 4478 ), ])

comp.preds <- predict(comp.model.final)
comp.preds

ggplot(data = comp[-c(1441, 1701, 3784, 4478 ), ], aes(y = comp.preds, x = price )) +
  labs(x = "Actual", y = "Predicted") +
  geom_point(color = "blue") +
  ggtitle("Actual vs Predicted Values")

hist(residuals(comp.model.final), ylim = c(0, 2000))
                            