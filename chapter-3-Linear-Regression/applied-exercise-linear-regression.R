# 8. This question involves the use of simple linear regression on the Auto data set.
library(ISLR2) # package that contains the datasets

head(Auto)

# a) use the lm() function to perform a simple linear regression 
# with mpg as response and horsepower as the predictor
lm.fit <- lm(mpg ~ horsepower, data=Auto)
# use the summary() function to print the results. Comment on the output
summary(lm.fit)
# R^2: 0.61
# The coefficient for predictor (horsepower) is negative indicating a negative linear relationship
# For bigger values of horsepower, mpg decreases
# The p-value for the coefficient of horsepower is close to zero indicating that the predictor is significant

# i.) Is there a relationship between the predictor and the response?
# There is a strong linear relationship between horsepower and mpg. ]
# The p-value of the regression coefficient is close to zero indicating that the predictor is a good fit
# H_0: B1 = 0 can be rejected

# ii.) How strong is the r/ship between the predictor and the response?
# The relationship is strong. The R^2 of the model is 0.61. 
# The relationship captures 61% of the variability in mpg

# iii.) Is the r/ship between the predictor and the response positive or negative
# The coefficient of the predictor is -0.158 indicating a negative relationship.
# For one unit increase in horsepower, there is a -0.158 decrease in mpg

# iv.) What is the predicted mpg associated with a horsepower of 98?
# What are the associated 95% confidence and prediction intervals?
# ?predict
predict(lm.fit, data.frame(horsepower=c(98)), interval='confidence', level=0.95)
# predicted mpg: 24.467
# predicted mpg confidence interval: lower: 23.97, upper=24.96
predict(lm.fit, data.frame(horsepower=c(98)), interval='prediction', level=0.95)
# prediction interval: lower: 14.8094, upper: 34.125

# b.) Plot the response and the predictor.
# Use the abline() function to display the ols regression line
plot(
  x=Auto$horsepower, y=Auto$mpg,
  pch="+", xlab="Horsepower",
  ylab="MPG",
  main="MPG against Horsepower"
  )
abline(lm.fit, col="red", lwd=3)

# c.) use the plot() function to produce diagnostic plots of the ols regression line.
# Comment on any problems you see with the fit.
par(mfrow=c(2,2))
plot(lm.fit)
# There is a discernible pattern of the residuals 
# indicating that the model has not explained all the variability

#######################################################
# 9. This question involves the use of multiple linear regression on the Auto data set.

# a.) Produce a scatterplot matrix which includes all of the variables in the data set.
pairs(Auto)

# b.) Compute the matrix of correlations between the variables using the function cor()
# Exclude the name variable which is qualitative

# omit the missing data to avoid problems while computing correlations
Auto <- na.omit(Auto)
names(Auto) # get order of variables
# omit the names column; the last column
cor(Auto[,1:8])

# c.) Ue the lm() function to perform a multiple linear regression with mpg as the response
# and all other variables except name as the predictors
mlm.fit <- lm(mpg ~ . - name, data=Auto)
summary(mlm.fit)
# R^2: 0.8215
# in the multiple linear regression model horsepower is not a significant predictor with a high p-value

# i.) Is there a relationship between the predictors and the response?
# There is a significant relationship between mpg and other predictors.
# This can be seen from a high F-statistic of 252.4
# The RSE of the model is 3.328 when the mean of mpg is 23.446
# This leads to an error of 3.328/23.446 = 0.142 i.e 14.2% error

# ii.) Which predictors appear to have a statistically significant relationship to the response?
# displacement: p-value of 0.08
# weight: p-value close to zero
# year: p-value close to zero
# origin: p-value close to zero

# iii.) What does the coefficient for the year variable suggest?
# The coefficient is 0.75. This indicates a positive linear relationship with mpg
# For every increase in one year, mpg increases by 0.75

# d.) use the plot() function to produce diagnostic plots.
# Comment on any problems you see with the fit.
par(mfrow=c(2,2))
plot(mlm.fit)

# there is little discernible pattern in the residuals of the fit
# There are some high leverage points according to the leverage plot

# e.) Use the * and : symbols to fit linear regression models with interaction effects.
# Do any interactions appear to be statistically significant?
mlm.fit1 <- lm(mpg ~ . -name + year:origin, data=Auto)
summary(mlm.fit1)

mlm.fit2 <- lm(mpg ~ . -name + horsepower:acceleration,data=Auto)
summary(mlm.fit2)
# R^2 : 0.84
# p-value very close to zero indicating the interaction is significant
summary(lm(mpg ~ . -name + cylinders:displacement + horsepower:acceleration, data=Auto))
# R^2: 0.85
# p-value for interaction terms is very low indicating interactions to be significant
summary(lm(mpg ~ displacement*horsepower + weight + year + origin, data=Auto))

# f.) Try a few different transformations of the variables, such as log(X), sqrt(X), X^2

summary(lm(mpg~weight+displacement+log(horsepower)+year+origin, data=Auto))
summary(lm(mpg~weight+horsepower+I(horsepower^2)+year+origin, data=Auto))
# R^2: 0.8506
summary(lm(mpg~log(weight)+displacement+horsepower+year+origin, data=Auto))

summary(lm(mpg~ weight + I(weight^2)+horsepower+year+origin, data=Auto))
# R^2: 0.855
summary(lm(mpg~sqrt(weight)+horsepower+year+origin, data=Auto))

##############################################################################
# 10. This question should be answered using the Carseats data set

# a.) Fit a multiple regression model to predict Sales using Price, Urban and US
mlm.fit <- lm(Sales ~ Price+Urban+US, data=Carseats)
summary(mlm.fit)

# b. Provide an interpretation of each coefficient in the model.
# Price: -0.054 negative linear relationship. If price increases by 1 unit, Sales will decrease by 54 units
# UrbanYes: -0.0219 If Yes for urban we get fewer sales in urban locations
# USYes: 1.2006 If the store is in the US we get more sales since there is positive relationship

# c) Write the model in equation form

# d) For which of the predictors can you reject the null hypothesis H_0: B_j = 0
# For Price and USYes since their p-values are close to zero

# e.) On the basis of your previous response fit a smaller model 
# that only uses the predictors for which there is evidence of association with outcome
mlm_small.fit <- lm(Sales ~ Price + US, data=Carseats)
summary(mlm_small.fit)
# R^2 remains constant. 
# The F-Statistic increases for the smaller model showing more confidence in the significance of the predictors

# g.) Using the model from (e), obtain 95% confidence intervals for coefficients
confint(mlm_small.fit)
# none of them have 0 in the confidence interval showing significance

# h.) Is there evidence of outliers or high leverage observations in the model from (e)?
plot(mlm_small.fit)
plot(hatvalues(mlm_small.fit))
# there are high leverage points 

###############################################################################
# 11. In this problem we will investigate the t-statistic for the null hypothesis
# H_0: B = 0 in simple linear regression without an intercept. 
# To begin, we generate a predictor x and a response y as follows:

set.seed(1)
x <- rnorm(100)
y <- 2 * x * rnorm(100)

# a.) perform a simple linear regression of y onto x, without an intercept.
# (You can perform regression without an intercept using the command lm(y~x+0))

lm.fit <- lm(y ~ x+0)
summary(lm.fit)
# report the coefficient estimate B, and the t-statistic and p-value associated with the H_0
# comment on these results
# coefficient of B: -0.4508
# standard error: 0.1573
# t-statistic: -2.866
# p-value: 0.00508
# comment: the coefficient is negative indicating a negative relationship of x and y
# the p-value is close to zero indicating that the predictor is significant regressor

# b.) Now perform a simple linear regression of x onto y without an intercept,
# and report the coefficient estimate, its standard error, and the corresponding 
# t-statistic and p-values associated with the null hypothesis H_0: B = 0
lm.fit1 <- lm(x ~ y+0)
summary(lm.fit1)

# coefficient of B: -0.1699
# standard error: 0.0593
# t-statistic: -2.866
# p-value: 0.00508
# comment: the coefficient B is negative indicating a negative relationship

# c.) What is the relationship between the results obtained in (a) and (b)
# the coefficient B for X~Y+0 and Y~X+0 is different but has same sign
# the standard error of both regressions is different
# the t-statistic and the p-value of the regressions is similar and show that
# both X and Y are significant predictors of each other

# f.) In R, show that when regression is performed with an intercept,
# the t-statistic for H_0:B_1 = 0 is the same for the regression of y onto x
# as it is for the regression of x onto y.
lm.fit2 <- lm(y ~ x) # y onto x
summary(lm.fit2)
# intercept: 0.107
# coefficient: -0.16945
# standard error: 0.05914
# t-statistic: -2.865
# p-value: 0.0051

lm.fit3 <- lm(x ~ y) # x onto y
summary(lm.fit3)
# intercept: 0.03974
# coefficient: -0.4561
# standard error: 0.15919
# t-statistic: -2.865
# p-value: 0.0051

# the t-statistic and the p-value is similar when the regression is done with an intercept

##############################################################################
# 12. This problem involves a simple linear regression without an intercept
# Under what circumstances is the coefficient estimate for the regression of X onto Y
# the same as the coefficient estimate for the regression of Y onto X?

# from the given formula, it can be shown that for coefficients to be equal
# then the sum x_i^2 has to be equal to sum y_i^2: sum(x_i^2)=sum(y_i^2)

# b.) Generate an example with n=100 observations 
# in which the coefficient estimate for the regression of X onto Y 
# is different from the coefficient estimate for the regression of Y onto X
x <- rnorm(100)
y <- 2 * rnorm(100, mean=5)
lm.fit <- lm(y ~ x+0)
summary(lm.fit)
# coefficient: 0.4951
# std error: 1.0037
# t-statistic: 0.493
# p-value: 0.623

lm.fit1 <- lm(x ~ y+0)
summary(lm.fit1)
# coefficient: 0.00495
# std error: 0.010039
# t-statistic: 0.493
# p-value: 0.623

# as seen from above summaries, the coefficient is different

# c.) Generate an example with n=100 observations
# in which the coefficient estimate for the regression of X onto Y 
# is the same as the coefficient estimate for the regression of Y onto X

# we need to ensure that x_i^2 == y_i^2
x <- 1:100
y <- 100:1

lm.fit <- lm(y ~ x+0)
summary(lm.fit)
# comments
# coefficient: 0.5075
# standard error: 0.0866
# t-statistic: 5.86

lm.fit1 <- lm(x ~ y+0)
summary(lm.fit1)
# coefficient: 0.5075
# standard error: 0.0866
# t-statistic: 5.86
# comment: the coefficient for regression of x on y is equal to coefficient of regression of y on x


################################################################################
# 13. In this exercise you will create some simulated data and will fit simple linear regression models to it.
# make sure to use set.seed(1)

set.seed(1)
# a.) using the rnorm() function, create a vector, x, 
# containing 100 observations drawn from a N(0,1) distribution. This represents feature X
x <- rnorm(100, mean=0, sd=1)

# b.) Using the rnorm() function, create a vector eps, 
# containing 100 observations drawn from a N(0, 0.25) distribution
# a normal distribution with mean zero and variance 0.25
eps <- rnorm(100, mean=0, sd=0.5)

# c.) using x and eps, generate a vector y according to the model
# Y = -1 + 0.5X + e
# what is the length of the vector y?
# what are the values of B_0 and B_1 in this linear model
y <- -1 + 0.5*x + eps
length(y)
# length of y : 100
# B_0: -1
# B_1: 0.5

# d.) Create a scatterplot displaying the r/ship between x and y.
plot(x, y)
# comment: The scatterplot exhibits a linear relationship between x and y.
# As x increases the values of y tend to increase in a linear fashion

# e.) Fit a least squares linear model to predict y using x.
# Comment on the model obtained
# how do B_0 estimate and B_1 estimate compare to B_0 and B_1
lm.fit <- lm(y ~ x)
summary(lm.fit)
# B_0: -1.02
# B_1: 0.4995
# estimated coefficients are very close to actual coefficients of the model y above

# f.) Display the least squares line on the scatterplot obtained in (d.)
# draw the population regression line on the plot, in a different color.
# Use the legend() command to create an appropriate legend
plot(x,y)
abline(lm.fit)
abline(a=-1, b=0.5, col="blue", lwd=2)
legend("bottomleft", c("Blue: Population", "Black: OLS Regression"))

# g.) Fit a polynomial regression model that predicts y using x and x^2.
# Is there evidence that the quadratic term improves the model fit? 
lm.fit_quad <- lm(y ~ x + I(x^2))
summary(lm.fit_quad)

# The quadratic term does not improve model fit.
# It has a p-value of 0.164 which is very high and we fail to reject the null hypothesis H_0: B_2=0

# h.) Repeat a) - f) after modifying the data generation process
# in such a way that there is less noise in the data.
# You can do this by decreasing the variance of the normal distribution used to generate the error term


# i.) Repeat a) - f) after modifying the data generation process
# in such a way that there is more noise in the data. 
# You can do this by increasing the variance of the normal distribution used to generate the error term


# j.) What are the confidence intervals for B_0 and B_1
# based on the original data set, the noisier data set, and the less noisy data set?

##########################################################################################
# 14. This problem focuses on the collinearity problem