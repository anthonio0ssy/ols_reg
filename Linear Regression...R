#Linear Regression

library(readxl)
library(lmtest)
new_data <- read.csv("C:/Users/HP 15/Documents/Linear Regression/wage1.csv")
attach(new_data)

#define variables
Y <- cbind(wage)
X <- cbind(educ)
X1 <- cbind(educ,exper)

#correlation
cor(Y,X1)

#plotting the data on a scatter diagram
plot(Y ~ X,data = new_data )

#simple linear regression
olsreg1 <- lm(Y ~ X)
summary(olsreg1)
confint(olsreg1,level = 0.95)
anova(olsreg1)

#plotting the regression line
abline(olsreg1)

#predicted values for dependent variable
Yhat <- fitted(olsreg1)
summary(Yhat)

#residuals
ehat <- resid(olsreg1)
summary(ehat)
plot(ehat ~ X)

#multiple regression 
olsreg2 <- lm(Y ~X1)
summary(olsreg2)
confint(olsreg2,level = 0.95)
anova(olsreg2)

#predicted values for dependent variable
Yhat1 <- fitted(olsreg2)
summary(Yhat1)

#residuals
ehat1 <- resid(olsreg2)
summary(ehat1)


