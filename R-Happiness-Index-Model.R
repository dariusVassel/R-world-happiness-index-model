#PACKAGES ADD
library(readr)
library(ggplot2)
library(datasets)
library(Hmisc)

world.happiness <- read.csv("2015.csv")

#SUMMARY
summary(world.happiness) 
str(world.happiness)
#OBSERVATION:
#There are no NA's or inaccurate observations so we do not need to screen for accuracy.

#Checking the degrees of freedom for the mahalanobis cutoff. 
#Summarizing mahal scores that are greater than the cutoff. 
#Checking the cut off score for our Mahalanobis measure.

mahal = mahalanobis(world.happiness[, 3:12], colMeans(world.happiness[, 3:12]), cov(world.happiness[, 3:12], use = "pairwise.complete.obs"))

cutoff = qchisq(1-0.001, ncol(world.happiness[, 3:12]))
cutoff

summary(mahal < cutoff)

#Plotting the variable Economy GDP per Capita as the predictor of the Happiness Score
#Plotting the variable Life Expectancy as the predictor of the Happiness Score

#Graphing the relationship between Economy GDP per Capita and Happiness Score

m1 = lm(world.happiness$Happiness.Score ~ world.happiness$Economy..GDP.per.Capita.)
summary(m1)

plot(world.happiness$Economy..GDP.per.Capita., world.happiness$Happiness.Score)

{plot(world.happiness$Economy..GDP.per.Capita.~ world.happiness$Happiness.Score)
  abline(lm(world.happiness$Economy..GDP.per.Capita.~ world.happiness$Happiness.Score))}

#OBSERVATION- The relationship is linear between the Economy of a country and the Happiness Score

#Looking at the correlation coefficient to quantify the strength of the relationship
cor(world.happiness$Economy..GDP.per.Capita., world.happiness$Happiness.Score)
#Correlation coefficient for happiness score and Economy is 0.781

{plot(world.happiness$Health..Life.Expectancy. ~ world.happiness$Happiness.Score)
  abline(lm(world.happiness$Health..Life.Expectancy. ~ world.happiness$Happiness.Score))}
#OBSERVATION- The relationship is linear


#Looking at the correlation coefficient to quantify the strength of the relationship

cor(world.happiness$Health..Life.Expectancy., world.happiness$Happiness.Score)

#The correlation coefficient for happiness score and life expectancy is 0.724

#Plotting the correlation of the world happiness score against the independent variables
corrplot::corrplot(cor(world.happiness[, 3:12]))

#plot_ss(x = world.happiness$Economy..GDP.per.Capita., y = world.happiness$Happiness.Score)

#plot_ss(x = world.happiness$Economy..GDP.per.Capita., y = world.happiness$Happiness.Score, showSquares = TRUE)

#Fitting the linar model (regression line)

m2 = lm(Happiness.Score ~ Economy..GDP.per.Capita., data = world.happiness)

summary(m2)



#OBSERVATION-
  
#The Cofficient's table in the summary fuction of m2 shows the linear model's y-intercept and the coefficient of Economy GDP per Capita.
#Therefore the least squares regression line for the linear model of Happiness Score is:
#Happiness Score = 3.499 + 2.218* Economy..GDP.per.capita
  
#Multiple R-squared representsthe proportion of variability in the response variable that is explained by the explanatory variable, 
#For this model 60.9% of the variability in the Happiness Score is explained by Economy GDP Per Capita.

#Checking to see if the linear model is reliable

#The relationship between Happiness Score and Economy GDP per Capita was linear 
#We will now verify this condition with the residuals vs GDP per Capita

{plot(m1$residuals ~ world.happiness$Economy..GDP.per.Capita.)
  abline(h = 0, lty = 3)}

hist(m1$residuals)

{qqnorm(m1$residuals)
  qqline(m1$residuals)}

#Checking the best predictor for Happiness Score
summary(lm(Happiness.Score ~ Economy..GDP.per.Capita., data = world.happiness))
#Multiple R Squared = 60.1%

summary(lm(Happiness.Score ~ Family, data = world.happiness))
#Multiple R Squared = 54.9%

summary(lm(Happiness.Score ~ Freedom, data = world.happiness))
#Multiple R Squared = 32.3%

summary(lm(Happiness.Score ~ Generosity, data = world.happiness))
#Multiple R Squared = 0.33%

summary(lm(Happiness.Score ~ Region, data = world.happiness))
#Multiple R Squared = 60.1%

summary(lm(Happiness.Score ~ Health..Life.Expectancy., data = world.happiness))
#Multiple R Squared = 52.5%

summary(lm(Happiness.Score ~ Trust..Government.Corruption., data = world.happiness))
#Multiple R Squared = 15.6%

summary(lm(Happiness.Score ~ Dystopia.Residual, data = world.happiness))
#Multiple R Squared = 28.1%

#Determining the best model fit
best.model.fit = lm(Happiness.Score ~ Economy..GDP.per.Capita. + Family + Region + Health..Life.Expectancy., data = world.happiness)
summary(best.model.fit)

best.model.fit.2 = lm(Happiness.Score ~ Economy..GDP.per.Capita. + Family + Region + Health..Life.Expectancy.+  Freedom + Trust..Government.Corruption. + Generosity + Dystopia.Residual, data = world.happiness)
summary(best.model.fit.2)

{qqnorm(best.model.fit$residuals)
  qqline(best.model.fit$residuals)}

{qqnorm(best.model.fit.2$residuals)
  qqline(best.model.fit.2$residuals)}

best.model.fit.3 = lm(Happiness.Score ~ Economy..GDP.per.Capita. + Family + Region + Health..Life.Expectancy.+  Freedom + Trust..Government.Corruption. + Dystopia.Residual, data = world.happiness)
summary(best.model.fit.3)

{qqnorm(best.model.fit.3$residuals)
  qqline(best.model.fit.3$residuals)}