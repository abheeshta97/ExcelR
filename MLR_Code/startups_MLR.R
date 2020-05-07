#50 Startups - MLR
#Predictions of Profit
pacman::p_load(corpcor, psych, corrplot, ggpubr, 
               dplyr, RColorBrewer, sjstats, car)
startup <- read.csv(file.choose())

startup.original <- startup
startup <- select(startup, -State)
View(startup)
head(startup.original, 10)

plot(startup)

startup_cor <- cor(startup)
corrplot(startup_cor, method = "number", col=brewer.pal(n=4, name="Blues"))
pairs(startup) #Scatter Plots

attach(startup)

#Model based on all columns
startup.model <- lm(Profit ~., data = startup)
summary(startup.model)
# Residual standard error: 9232 on 46 degrees of freedom
# Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9475 
# F-statistic:   296 on 3 and 46 DF,  p-value: < 2.2e-16
sigma(startup.model)/mean(Profit) #0.08242226
vif(startup.model)
# R.D.Spend  Administration Marketing.Spend 
# 2.468903        1.175091        2.326773 

#Model based on R&D spending and Marketing Spending 
startup.model.rm <- lm(Profit~R.D.Spend+Marketing.Spend, data = startup)
summary(startup.model.rm)
# Residual standard error: 9161 on 47 degrees of freedom
# Multiple R-squared:  0.9505,	Adjusted R-squared:  0.9483 
# F-statistic: 450.8 on 2 and 47 DF,  p-value: < 2.2e-16
sigma(startup.model.rm)/mean(Profit) #0.08178511
vif(startup.model.rm)
# R.D.Spend Marketing.Spend 
# 2.103206        2.103206


#Model based on R&D Spend and Admin
startup.model.ra <- lm(Profit ~ R.D.Spend+Administration, data = startup)
summary(startup.model.ra)
# Residual standard error: 9402 on 47 degrees of freedom
# Multiple R-squared:  0.9478,	Adjusted R-squared:  0.9456 
# F-statistic: 426.8 on 2 and 47 DF,  p-value: < 2.2e-16
sigma(startup.model.ra)/mean(Profit) #0.08393346
vif(startup.model.ra)
# R.D.Spend Administration 
# 1.062183       1.062183 

startup.model.am <- lm(Profit ~ Administration + Marketing.Spend, 
                       data = startup)
summary(startup.model.am)
# Residual standard error: 25710 on 47 degrees of freedom
# Multiple R-squared:  0.6097,	Adjusted R-squared:  0.5931 
# F-statistic: 36.71 on 2 and 47 DF,  p-value: 2.496e-10
sigma(startup.model.am)/mean(Profit) #0.2295304
vif(startup.model.am)
# Administration Marketing.Spend 
# 1.001035        1.001035 

startup.model.r <- lm(Profit ~ R.D.Spend, data = startup)
summary(startup.model.r)

par(mfrow = c(2,2))
plot(startup.model.ra)
par(mfrow = c(1,1))

influenceIndexPlot(startup.model.ra, id.n = 6)
influencePlot(startup.model.ra, id.n = 6)

#Test
summary(lm(Profit~R.D.Spend+Administration, data = startup[-c(38,49,50), ])) #0.9596
summary(lm(Profit~R.D.Spend+Administration, data = startup[-c(37,38,49,50), ])) #0.9631
summary(lm(Profit~R.D.Spend+Administration, data = startup[-c(37,49,50), ])) #0.9631

startup.model.final <- lm(Profit~R.D.Spend+Administration, data = startup[-c(37,49,50), ])
summary(startup.model.final)
# Residual standard error: 7109 on 44 degrees of freedom
# Multiple R-squared:  0.9647,	Adjusted R-squared:  0.9631 
# F-statistic: 601.7 on 2 and 44 DF,  p-value: < 2.2e-16


startup_preds <- predict(startup.model.final)
startup_preds
startup_preds_conf <- predict(startup.model.final, interval = "confidence")
startup_preds_conf
startup_preds_int <- predict(startup.model.final, interval = "prediction")
startup_preds_int
hist(residuals(startup.model.final))

