#Toyota Corolla - MLR
#ToyotaCorolla.csv
library(corrplot)
library(RColorBrewer)
library(sjstats)
library(car)
library(ggiraphExtra)
library(moonBook)
library(plyr)
library(ggplot2)

toyota.original <- read.csv(file.choose()) #ToyotaCorolla.csv
View(toyota.original)
toyota <- subset(toyota.original, select = c(3, 4, 7, 9, 13, 14, 16:18))
head(toyota)

toyota.cor <- cor(toyota)
corrplot(toyota.cor, method = "number", col=brewer.pal(n=8, name="Dark2"))
attach(toyota)

toyota.model <- lm(Price ~., data = toyota)
summary(toyota.model)
# Residual standard error: 1342 on 1427 degrees of freedom
# Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863 
# F-statistic:  1131 on 8 and 1427 DF,  p-value: < 2.2e-16
sigma(toyota.model)/mean(Price) #0.1251043
vif(toyota.model)

toyota.model.akhgqw <- lm(Price ~ Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data = toyota)
summary(toyota.model.akhgqw)
# Residual standard error: 1342 on 1429 degrees of freedom
# Multiple R-squared:  0.8636,	Adjusted R-squared:  0.863 
# F-statistic:  1508 on 6 and 1429 DF,  p-value: < 2.2e-16
sigma(toyota.model.akhgqw)/mean(Price) #0.1250958


toyota.model.akhw <- lm(Price ~ Age_08_04+KM+HP+Weight, data = toyota)
summary(toyota.model.akhw)
# Residual standard error: 1350 on 1431 degrees of freedom
# Multiple R-squared:  0.8618,	Adjusted R-squared:  0.8614 
# F-statistic:  2230 on 4 and 1431 DF,  p-value: < 2.2e-16
sigma(toyota.model.akhw)/mean(Price) #0.125837

toyota.model.ak <- lm(Price ~ Age_08_04+ KM, data = toyota)
summary(toyota.model.ak)

toyota.model.aw <- lm(Price ~ KM + Weight, data = toyota)
summary(toyota.model.aw)

toyota.model.w <- lm(Price ~ Weight, data = toyota)
summary(toyota.model.w)

toyota.model.ha <- lm(Price ~ HP + Age_08_04, data = toyota)
summary(toyota.model.ha)
# Residual standard error: 1620 on 1433 degrees of freedom
# Multiple R-squared:  0.8008,	Adjusted R-squared:  0.8005 
# F-statistic:  2880 on 2 and 1433 DF,  p-value: < 2.2e-16

toyota.model.a <- lm(Price ~ Age_08_04, data = toyota)
summary(toyota.model.a)

toyota.model.akw <- lm(Price ~ Age_08_04+KM+Weight, data = toyota)
summary(toyota.model.akw)
# Residual standard error: 1415 on 1432 degrees of freedom
# Multiple R-squared:  0.8481,	Adjusted R-squared:  0.8478 
# F-statistic:  2665 on 3 and 1432 DF,  p-value: < 2.2e-16
sigma(toyota.model.akw)/mean(Price) #0.1318674
vif(toyota.model.akw)

toyota.model.k <- lm(Price ~ KM, data = toyota)
summary(toyota.model.k)

toyota.model.w <- lm(Price ~ Weight, data = toyota)
summary(toyota.model.w)

toyota.model.qwa <- lm(Price ~ Quarterly_Tax+Weight+Age_08_04, data = toyota)
summary(toyota.model.qwa)
# Residual standard error: 1569 on 1432 degrees of freedom
# Multiple R-squared:  0.8134,	Adjusted R-squared:  0.813 
# F-statistic:  2080 on 3 and 1432 DF,  p-value: < 2.2e-16
sigma(toyota.model.qwa)/mean(Price) #0.1461681
vif(toyota.model.qwa)

toyota.model.chkwq <- lm(Price ~ cc+HP+KM+Weight+Quarterly_Tax, data = toyota)
summary(toyota.model.chkwq)

toyota.model.chkw <- lm(Price ~ cc+HP+KM+Weight, data = toyota)
summary(toyota.model.chkw)

toyota.model.cdg <- lm(Price ~ cc+Doors+Gears, data = toyota)
summary(toyota.model.cdg)

toyota.model.akwq <- lm(Price ~ Age_08_04 + KM + Weight + Quarterly_Tax, data = toyota)
summary(toyota.model.akwq)
# Residual standard error: 1414 on 1431 degrees of freedom
# Multiple R-squared:  0.8484,	Adjusted R-squared:  0.8479 
# F-statistic:  2001 on 4 and 1431 DF,  p-value: < 2.2e-16
sigma(toyota.model.akwq)/mean(Price) #0.1318025
vif(toyota.model.akwq)

toyota.model.akwqd <- lm(Price ~ Age_08_04 + KM + Weight + Quarterly_Tax + Doors, data = toyota)
summary(toyota.model.akwqd)

toyota.model.akh <- lm(Price ~ Age_08_04+KM+HP, data = toyota)
summary(toyota.model.akh)
# Residual standard error: 1581 on 1432 degrees of freedom
# Multiple R-squared:  0.8103,	Adjusted R-squared:  0.8099 
# F-statistic:  2039 on 3 and 1432 DF,  p-value: < 2.2e-16

par(mfrow = c(2,2))
plot(toyota.model.akhgqw)
par(mfrow = c(1,1))

influenceIndexPlot(toyota.model.akhgqw, id.n = 6)
influencePlot(toyota.model.akhgqw, id.n = 6)

summary(lm(Price ~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data = toyota[-c(222, 602, 961), ]))
toyota.model.final <- lm(Price ~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight, data = toyota[-c(222, 602, 961), ])
summary(toyota.model.final)
toyota.model.final

plot(toyota.model.final)

toyota.preds <- predict(toyota.model.final)
toyota.preds
plot(toyota.preds ~ toyota[-c(222, 602, 961),])

ggplot(data = toyota[-c(222, 602, 961),], aes(y = toyota.preds, x = Price )) +
  labs(x = "Actual", y = "Predicted") +
  geom_point(color = "blue") +
  ggtitle("Actual vs Predicted Values")

hist(residuals(toyota.model.final), ylim = c(0, 500))
