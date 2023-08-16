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
jpeg("chapter-3-Linear-Regression/ols-simulated-data.jpeg")
plot(x,y)
abline(lm.fit)
abline(a=-1, b=0.5, col="blue", lwd=2)
legend("bottomleft", c("Blue: Population", "Black: OLS Regression"))
dev.off()
# g.) Fit a polynomial regression model that predicts y using x and x^2.
# Is there evidence that the quadratic term improves the model fit? 
lm.fit_quad <- lm(y ~ x + I(x^2))
summary(lm.fit_quad)

# The quadratic term does not improve model fit.
# It has a p-value of 0.164 which is very high and we fail to reject the null hypothesis H_0: B_2=0

# h.) Repeat a) - f) after modifying the data generation process
# in such a way that there is less noise in the data.
# You can do this by decreasing the variance of the normal distribution used to generate the error term
set.seed(1)
x <- rnorm(100, mean=0, sd=1)
eps <- rnorm(100, mean=0, sd=0.05)
y <- -1 + 0.5*x + eps
plot(x, y)
lm.fit <- lm(y ~ x)
summary(lm.fit)
# less noisier data leads to a high R^2 of 0.9888
jpeg("chapter-3-Linear-Regression/ols-less-noisier-data.jpeg")
plot(x,y,main="Less Noisier Data")
abline(lm.fit)
abline(a=-1, b=0.5, col="blue", lwd=2)
legend("bottomleft", c("Blue: Population", "Black: OLS Regression"))
dev.off()
# i.) Repeat a) - f) after modifying the data generation process
# in such a way that there is more noise in the data. 
# You can do this by increasing the variance of the normal distribution used to generate the error term

set.seed(1)
x <- rnorm(100, mean=0, sd=1)
eps <- rnorm(100, mean=0, sd=5)
y <- -1 + 0.5*x + eps
plot(x, y)
lm.fit <- lm(y ~ x)
summary(lm.fit)
jpeg("chapter-3-Linear-Regression/more-noisy-data.jpeg")
plot(x,y,main="More Noisy Data")
abline(lm.fit)
abline(a=-1, b=0.5, col="blue", lwd=2)
legend("bottomleft", c("Blue: Population", "Black: OLS Regression"))
dev.off()

# j.) What are the confidence intervals for B_0 and B_1
# based on the original data set, the noisier data set, and the less noisy data set?
# Confidence Interval Original Data:
# B_0: 2.5%: -1.115  97.5%: -0.923
# B_1: 2.5%: 0.3926 97.5%: 0.606
# Confidence Interval Less Noisier Data:
# B_0: 2.5%: -1.0115 97.5%: -0.9923
# B_1: 2.5%: 0.48926 97.5%: 0.5106
# Confidence Interval More Noisier Data:
# B_0: 2.5%: -2.1508 B_1: -0.226
# B_1: 2.5%: -0.5742 B_1: 1.5636
##########################################################################################
# 14. This problem focuses on the collinearity problem

# a.) Perform the following commands in R
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100) # creates a linear model in which y is a function of x1 and x2
# write out y the regression coefficients of y
# y <- x+2x1 +0.3x2 + e 
# coefficients: B_1:2, B_2:0.3

# b.) What is the correlation between x1 and x2?
# Create a scatterplot displaying the relationship between the variables
cor(x1,x2)
# correlation: 0.835
plot(x1, x2)

# c.) fit a least squares regression to predict y using x1 and x2.
# Describe the results obtained
lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
# comments:
# intercept B_0: 2.1305
# coefficient x1 B_1: 1.4396 
# coefficient x2 B_2: 1.0097
# the estimated coefficients are not equal or close to the real model coefficients
# can you reject the null hypothesis H_0:B_1=0 
# we can reject this null hypothesis since the p-value of 0.0487 is less than 0.05

# can you reject the null hypothesis H_0:B_2=0
# we fail to reject the null hypothesis since the p-value of 0.375 is high
# and thus the coefficient B_1 might be insignificant

# d.) Now fit a least squares regression to predict y using only x1.
lm.fit1 <- lm(y ~ x1)
summary(lm.fit1)
# intercept B_0: 2.1124
# coefficient B_1: 1.9759 , p-value: very close to zero
# We reject the null hypothesis H_0:B_1=0 since the p-value is virtually zero

# e.) now fit a least squares regression to predict y using only x2.
lm.fit2 <- lm(y ~ x2)
summary(lm.fit2)
# intercept B_0: 2.3899
# coefficient B_1: 2.8996, p-value: very close to zero
# can you reject the null hypothesis H_0:B_1=0
# we reject the null hypothesis because the p-value is close to zero indicating a significant predictor

# f.) Do the results obtained in (c)-(e) contradict each other? Explain you answer
# Yes the results contradict.
# In the multiple linear regression model, coefficient for x2 was seen to be insignificant.
# In the simple linear regression of y on x_2, it is seen that x_2 is significant.
# This is because when performing a linear regression with the two predictors x_1 and x_2,
# the change in y due to change in x2 is small since x1 is held at a constant value.
# This is because x1 and x2 are strongly correlated

# g.) Now suppose we obtain one additional observation, which was unfortunately mismeasured.
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

# Refit the linear models from (c)-(e) using the new data.
# What effect does this new observation have on each of the models?
# In each model, is this observation an outlier? A high leverage point or both?

lm.fit <- lm(y ~ x1 + x2)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
# shows that only x1 is statistically significant as a predictor

lm.fit1 <- lm(y ~ x1)
summary(lm.fit1)
plot(lm.fit1) # residuals shows a pattern
# x1 is statistically significant

lm.fit2 <- lm(y ~ x2)
summary(lm.fit2)
plot(lm.fit2)
# x2 is statistically significant

#################################################################################
# 15. This problem involves the Boston data set.
# We will now try to predict per capita crime rate using the other variables in the data.

# a.) For each predictor, fit a simple linear regression model to predict the response.
# Describe your results.
# In which of the model is there a statistically significant association between the predictor
# and the response. Create some plots to back up your assertions
library(ISLR2)
head(Boston)
?Boston
attach(Boston)
# crim column is the per capita crime rate by town. the response variable

# predictor 1, indus: proportion of residential land zoned for lots over 25,000 sq.ft
plot(zn, crim) # the relationship is not linear
lm.zn <- lm(crim ~ zn)
summary(lm.zn)
# R^2: 0.04
# coefficient zn: -0.07393, p-value: close to zero

# predictor 2, indus: proportion of non-retail business acres per town
plot(indus, crim) # the plot does not exhibit a linear relationship
lm.indus <- lm(crim ~ indus)
summary(lm.indus)
# R^2: 0.165
# coefficient indus: 0.50978, p-value: close to zero 

# predictor 3, chas: Charles River dummy variable
plot(chas, crim) # does not exhibit linear r/ship since its a categorical variable
lm.chas <- lm(crim ~ chas)
summary(lm.chas)
# R^2: 0.003
# coefficient chas: -1.8928, p-value: 0.209 greater than 0.05
# we fail to reject the null hypothesis H_0: B_1 = 0

# predictor 4, nox: nitrogen oxides concentration 
plot(nox, crim) # the plot indicates a weak linear relationship
lm.nox <- lm(crim ~ nox)
summary(lm.nox)
# R^2: 0.1772
# coefficient nox: 31.249, p-value: close to zero

# predictor 5, rm: average number of rooms per dwelling
plot(rm, crim)
lm.rm <- lm(crim ~ rm)
summary(lm.rm)
# R^2: 0.048
# coefficient rm: -2.684, p-value: close to zero

# predictor 6, age: proportion of owner occupied units built prior to 1940
plot(age, crim)
lm.age <- lm(crim ~ age)
summary(lm.age) 
# R^2: 0.1244
# coefficient age: 0.10779, p-value: close to zero


# predictor 7, dis: weighted mean of distances to five Boston employment centres
plot(dis, crim) # weak inverse linear r/ship
lm.dis <- lm(crim ~ dis)
summary(lm.dis)
# R^2: 0.144
# coefficient: -1.5509, p-value: very close to zero

# predictor 8, rad: index of accessibility to radial highways
plot(rad, crim) # very weak linear relationship
lm.rad <- lm(crim ~ rad)
summary(lm.rad) 
# R^2: 0.3913
# coefficient rad: 0.61791, p-value: very close to zero

# predictor 9, tax: full-value property-tax rate per $10,000
plot(tax, crim) # very weak linear relationship
lm.tax <- lm(crim ~ tax)
summary(lm.tax) 
# R^2: 0.3396
# coefficient tax: 0.029742, p-value: very close to zero

# predictor 10, ptratio: pupil-teacher ratio by town
plot(ptratio, crim) # very weak to no linear relationship
lm.ptratio <- lm(crim ~ ptratio)
summary(lm.ptratio)
# R^2 0.08407
# coefficient ptratio: 1.1520, p-value: very close to zero

# predictor 11, lstat: lower status of the population (percent)
plot(lstat, crim) # plot exhibits a linear relationship
lm.lstat <- lm(crim ~ lstat)
summary(lm.lstat)
abline(lm.lstat)
# R^2: 0.2076
# coefficient lstat: 0.54880, p-value: very close to zero

# predictor 12, medv: median value of owner-occupied homes in $1000s
plot(medv, crim) # plot shows a weak inverse linear relationship
lm.medv <- lm(crim ~ medv)
summary(lm.medv)
# R^2: 0.1508
# coefficient: -0.36316, p-value: very close to zero

# b.) Fit a multiple regression model to predict the response using all of the predictors.
# Describe your results
# For which predictors can we reject the null hypothesis H_0: B_j=0?
mlm.fit <- lm(crim ~ ., data=Boston)
summary(mlm.fit)
# R^2: 0.4493
# predictors we can reject H_0: B_j = 0
# zn: p-value: 0.015
# dis: p-value: 0.0037
# rad: p-value: very close to zero
# medv: p-value 0.00026

# c.) How do your results from (a) compare to your results from (b)
# Create a plot displaying the univariate regression coefficients from (a) on the x-axis,
# and the multiple regression coefficients from (b) on the y-axis

# The R squared has significantly improved in the multiple linear regression model.
# Some variables that were significant in the simple linear regression model
# are not significant in the multiple linear model

multi_coefs <- c(
  0.0457, -0.05835, -0.8254, -9.9576, 
  0.6289, -0.000848, -1.0122467, 0.612465, 
  -0.00378, -0.30407, 0.1388, -0.22006
  )
length(multi_coefs)

linear_coefs <- c(
  -0.07393, 0.50978, -1.8928, 31.249,
  -2.684, 0.10779, -1.5509, 0.61791,
  0.029742, 1.1520, 0.54880, -0.36316
) 
plot(linear_coefs, multi_coefs)

# d.) Is there evidence of non-linear association between any of the predictors and the response?
# To answer this question, for each predictor X, fit a model of the form:
# Y = B_0 + B_1X + B_2X^2 + B_3X^3 + e
# fit a higher order linear relationship on zn
lm.zn_poly <- lm(crim ~ zn + I(zn^2) + I(zn^3))
summary(lm.zn_poly)
# R^2: 0.058
# coefficient zn has p-value 0.002 significant in 1-st order
# coefficient for zn^2 and zn^3 are not significant and we fail to reject H_0:B_i=0

lm.indus_poly <- lm(crim ~ indus + I(indus^2) + I(indus^3))
summary(lm.indus_poly)
# R^2: 0.26
# Coefficients: indus has very low p-value
# coefficients for X^2 and X^3 are also significant with low p-values
# indicating a non linear relationship with indus poly


lm.chas_poly <- lm(crim ~ chas + I(chas^2) + I(chas^3))
summary(lm.chas_poly)
# coefficient for chas: not significant in first order
# coefficients for X^2 and X^3 are NA showing that they are insignificant

lm.nox_poly <- lm(crim ~ nox + I(nox^2) + I(nox^3))
summary(lm.nox_poly)
# R^2: 0.297
# coefficient for nox: very low p-value
# cooefficient for X^2 and X^3 are very low indicating a non linear relationship

lm.rm_poly <- lm(crim ~ rm + I(rm^2) + I(rm^3))
summary(lm.rm_poly)
# coefficient of rm: has very high p-value thus relationship not statistically significant
# coefficients of X^2 and X^3 are not significant showing no higher order relationship between rm and crim

lm.age_poly <- lm(crim ~ age + I(age^2) + I(age^3))
summary(lm.age_poly)
# R^2: 0.17
# coefficient for age: has high p-value thus no linear relationship
# coefficient for X^2  has p-value=0.047 indicating a statistically significant relationship for age^2
# coefficient for X^3 has a p-value=0.00668 indicating a strongly significant association in 3rd order polynomial

lm.dis_poly <- lm(crim ~ dis + I(dis^2) + I(dis^3))
summary(lm.dis_poly)
# R^2: 0.2778
# coefficient for dis: p-value very close to zero indicating significant association
# coefficients for X^2 and X^3 have p-values close to zero indicating strong non linear rship

lm.rad_poly <- lm(crim ~ rad + I(rad^2) + I(rad^3))
summary(lm.rad_poly)
# R^2: 0.4
# coefficient for rad: has p-value of 0.623 thus we fail to reject null hypothesis
# coefficients for X^2 and X^3 have p-values that are high thus strong evidence against non-linear relationship

lm.tax_poly <- lm(crim ~ tax + I(tax^2) + I(tax^3))
summary(lm.tax_poly)
# R^2: 0.369
# coefficient for tax has a p-value of 0.110 thus strong evidence against a linear relationship
# coefficients for X^2 and X^3 have high p-values indicating no association for high order polynomial relationship 

lm.ptratio_poly <- lm(crim ~ ptratio + I(ptratio^2) + I(ptratio^3))
summary(lm.ptratio_poly)
# R^2: 0.1138
# coefficient of ptratio has p-value of 0.00303 so we reject the null hypothesis B_1=0
# coefficients of ptratio^2 and ptratio_3 have p-values less than 0.05 indicating strong non-linear association
lm.lstat_poly <- lm(crim ~ lstat + I(lstat^2) + I(lstat^3))
summary(lm.lstat_poly)
# R^2: 0.2179
# coefficient of lstat has a p-value of 0.3345 so we fail to reject null hypothesis H_0: B_1=0
# coefficients for lstat^2 and lstat^3 have high p-values indicating that the non-linear r/ship is not statistically significant

lm.medv_poly <- lm(crim ~ medv + I(medv^2) + I(medv^3))
summary(lm.medv_poly)
# R^2: 0.4202
# coefficient for medv: has p-value close to zero indicating strong statistical linear relationship
# coefficients for medv^2 and medv^3 have p-values close to zero indicating statistically significant non-linear associations