library(MASS)
library(ISLR2)

#########################################################
# Simple Linear Regression
#########################################################

# The ISLR2 library contains the Boston dataset, which records
# medv (median house value) for 506 census tracts in Boston.
# We seek to predict medv usng 12 predictors such as
# rm (average number of rooms per house),
# age (average age of houses), and
# lstat (percent of households with low socioeconomic status)

head(Boston)
# start by using the lm() function to fit a simple linear regression model
# with medv as the response and lstat as the predictor.
# Syntax: lm(y ~ x, data) where y:response, x:predictor

lm.fit <- lm(medv ~ lstat, data=Boston)
lm.fit

# if we attach Boston, we do not need to provide the data argument
attach(Boston)
lm.fit <- lm(medv ~ lstat)
lm.fit # provides basic information about the model 
summary(lm.fit) # provides more detailed information about the model 
# R^2: 0.54

# use the names() function to find out what other pieces of information are stored in lm.fit
names(lm.fit)
# we can extract these quantities by their names
lm.fit$coefficients
# it is safer to extract such quantities using specific functions e.g coef()
coef(lm.fit)

# obtain confidence interval for the coefficient estimates
confint(lm.fit)

# produce confidence intervals and prediction intervals 
# for the prediction of medv for a given value of lstat
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")

# plot mev and lstat along with the least squares regression line
plot(lstat, medv)
abline(lm.fit)
# there is evidence of non-linearity in the r/ship between medv and lstat

# The abline() function can be used to draw any line, not just the ols regression line
# To draw a line with intercept a and slope b, we type abline(a,b)
abline(lm.fit, lwd=3) # lwd argument adjusts the width of the regression line
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20)
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)

# examine some diagnostic plots
# four diagnostic plots are automatically produced by applying the plot() fn directly to output of lm
# this command will produce one plot at a time,
# and hitting Enter will generate the next plot
# However, its convenient to view all four plots together using par(mfrow=)

par(mfrow=c(2,2)) # divide output panel to two rows, each with two columns
plot(lm.fit)

# alternatively, we can compute the residuals from a linear regression fit using the residuals() function
# The function rstudent() will return studentized residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
# on the basis of residual plots, there is some evidence of non-linearity

# Leverage statistics can be computed for any number of predictors using the hatvalues() fn
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # identifies the index of the largest element of a vector.
# in this case it tells us which observation has the largest leverate statistic

###################################################################################
# Multiple Linear Regression
##################################################################################

# to fit a multiple linear regression model using least squares, use the lm() function
# Syntax: lm(y ~ x1 + x2 + x3), y: response, x1,x2,x3: predictors

mlm.fit <- lm(medv ~ lstat + age)
summary(mlm.fit) # produce coefficients for all predictors
# R^2: 0.55

# To perform a multiple linear regression model, you dont need to specify all predictors
mlm_all.fit <- lm(medv ~ ., data=Boston) # shorthand for using all other variables as regression predictors
summary(mlm_all.fit)
# R^2: 0.734

summary(mlm.fit)$r.sq # give us the R squared
summary(mlm_all.fit)$r.sq # gives us the R squared of model with all variables

summary(mlm.fit)$sigma # gives us the RSE
summary(mlm_all.fit)$sigma # RSE when using all variables as predictors

# The vif() function, part of car package, can be used to compute VIF (Variance Inflation Factor)
install.packages('car') 
library(car) 
vif(mlm.fit)

# What if we would like to perform a regression using all of the variables but one?
# In the above regression, age has a high p-value.
# Let's run a regression excluding age
mlm_all.fit1 <- lm(medv ~ . - age, data=Boston)
summary(mlm_all.fit1)

# indus also has a high p-value
# use the update() fn to remove the predictor
mlm_all.fit1 <- update(mlm_all.fit1, ~ . - indus)
summary(mlm_all.fit1)

########################################################################################
# Interaction Terms
########################################################################################

# Syntax: lstat:black tells R to include an interaction term between lstat and black
# Syntax: lstat*age simultaneously includes lstat, age and the interaction term lstat*age ss predictors
# Syntax: lstat*age is a shorthand for lstat + age + lstat:age

summary(lm(medv~ lstat * age, data=Boston))

###############################################################################
# Non-Linear Transformations of the Predictors
###############################################################################
# lm() function also accommodates non-linear transformations of predictors
# Give a predictor X, we can create a predictor X^2 using I(X^2)
# The function I() is needed since the ^ hs special meaning in a formula object

# let's perform a regression of medv onto lstat and lstat^2

lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
# R^2: 0.64
# Improves the R Squared of the model
# The near-zero p-value associated with the quadratic term suggests that it leads to model improvement.
# Use the anova() function to further quantify 
# the extent to which the quadratic fit is superior to the linear fit

anova(lm.fit, lm.fit2)
# The anova() function performs a hypothesis test comparing the two models.
# H_0: the two models fit the data equally well
# H_A: Model two is superior
# Here the F-statistic is 135 and the p-value is close to zero.
# The model containing the predictors lstat and lstat^2 is far superior
# to the model containing lstat as the only predictor
# Not surprising since the plot exhibited a non-linear relationship between medv and lstat

par(mfrow=c(2,2))
plot(lm.fit2) # residuals do not exhibit a discernible pattern

##############################
# Higher order polynomial regression
################################
# In order to create a cubic fit, we can include a predictor of the form I(X^3)
# However, this approach can get cumbersome for higher-order polynomials.
# A better approach includes using the poly() function to create the polynomial within lm()

# The following command produces a fifth-order polynomial
lm.fit5 <- lm(medv ~ poly(lstat, 5), data=Boston)
summary(lm.fit5)
# R^2: 0.68
# Reveals that including additional polynomial terms, 
# up to the fifth order, leads to an improvement in moel fit.

# The poly() function orthogonalizes the predictors: i.e
# the features output by this function are not simply a sequence of powers of the argument
# In order to obtain the raw polynomials from the poly() fn, the argument raw=TRUE has to be used

lm.fit5_raw <- lm(medv ~ poly(lstat, 5, raw=T), data=Boston)
summary(lm.fit5_raw)
# Output is similar to the orthogonalized polynomial

# try a log transformation
lm_log.fit <- lm(medv ~ log(rm), data=Boston)
summary(lm_log.fit)
# R^2: .4358

lm_log.fit1 <- lm(medv ~ log(lstat), data=Boston)
summary(lm_log.fit1)
# R^2 0.66

################################################################################
# Qualitative Predictors
################################################################################
# We will now examine the Carseats dataset
# We attempt to predict Sales (child car seat sales) in 400 locations
head(Carseats)
# Qualitative Variables: ShelveLoc, Urban, US
# Given a qualitative variable, R generates dummy variables automatically

# Fit a multiple regression model including some interaction terms
mlm_qual.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data=Carseats)
summary(mlm_qual.fit)
#R^2: 0.87
# The p-values for ShelveLoc variable are near zero indicating the qualitative variable is significant

# Use the contrasts() function to get the coding for dummy variables
contrasts(Carseats$ShelveLoc)
# R has created a ShelveLocGood dummy variable that takes on a value of 1 if the shelving location is good, 0 otherwise
# ShelveLocMedium dummy variable equals 1 if the shelving location is medium, and 0 otherwise
# A Bad shelving location takes a 0 for both of the two dummy variables

# the coefficient for ShelveLocGood is positive indicating higher sales associated with good shelving location
# the coefficient for ShelveLocMedium is lower than that of ShelveLocGood indicating less sales for medium shelving location

##################################################
# Writing Functions
################################################
# We provide a simple function LoadLibraries() that reads the ISLR2 and MASS libraries 
LoadLibraries <- function(){
  library(ISLR2)
  library(MASS)
  print("The libraries ISLR2 and MASS have been loaded.")
}

LoadLibraries # R will tell us what is in the function

LoadLibraries() # call the function
