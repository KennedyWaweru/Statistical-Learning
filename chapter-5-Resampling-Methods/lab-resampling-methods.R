################################################################################
# The Validation Set Approach
################################################################################

# We explore the use of the validation set approach in order to estimate the 
# test error rates that result from fitting various linear models on the Auto 
# data set.

library(ISLR2)
set.seed(1)
train <- sample(392, 196)

# begin by using the sample() function to split the set of observations into
# two halves, by selecting a random subset of 196 observations out of 
# the original 392 observations. 

# We then use the subset option in lm() to fit a linear regression using only
# the observations corresponding to the training set
lm.fit <- lm(mpg ~ horsepower, data=Auto, subset=train)
summary(lm.fit)

# use the predict() function to estimate the response for all 392 observations,
# and we use the mean() function to calculate the MSE of the 196 observations
# in the validation set. 
# Note that the -train index below selects only the observations that are not
# in the training set.
mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)
# 23.26601
# the test MSE for the linear regression fit is 23.27

# We use the poly() function to estimate the test error for the quadratic and 
# cubic regressions

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data=Auto, subset=train)
summary(lm.fit2)

mean((Auto$mpg - predict(lm.fit2, Auto))[-train]^2)
# 18.71646

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data=Auto, subset=train)
summary(lm.fit3)

mean( (Auto$mpg - predict(lm.fit3, Auto))[-train]^2 )
# 18.794

# These error rates are 18.72 and 18.79, respectively. 
# If we choose a different training set, then we obtain different errors on 
# the validation set

set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, data=Auto, subset=train)
summary(lm.fit)

mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)
# 25.7265

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data=Auto, subset=train)
summary(lm.fit2)

mean((Auto$mpg - predict(lm.fit2, Auto))[-train]^2)
# 20.43036

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data=Auto, subset=train)
summary(lm.fit3)

mean((Auto$mpg - predict(lm.fit3, Auto))[-train]^2)
# 20.385

# Using this split of observations into a training set and a validation set,
# we find that the validation set error rates for the models with linear,
# quadratic, and cubic terms are 25.73, 20.43, and 20.39 respectively.


################################################################################
# Leave-One-Out Cross-Validation
################################################################################

# The LOOCV estimate can be automatically computed for any generalized linear
# model using the glm() and cv.glm() functions. 
# If we use glm() to fit a model without passing in the family argument,
# then it performs linear regression, just like the lm() function.

glm.fit <- glm(mpg ~ horsepower, data=Auto)
coef(glm.fit)
# Intercept: 39.93586, horsepower: -0.1578

lm.fit <- lm(mpg ~ horsepower, data=Auto)
coef(lm.fit)
# Intercept: 39.93586, horsepower: -0.1578

# glm() and lm() will yield identical linear regression models.

# In this section we will perform linear regression using the glm() function 
# rather than the lm() function because glm() can be used together with cv.glm()

# The cv.glm() function is part of the boot library

library(boot)
glm.fit <- glm(mpg ~ horsepower, data=Auto) 
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
# 24.2315, 24.2311

# the cv.glm() function produces a list with several components.
# The two numbers in the delta vector contain the cross-validation results.
# Our cross-validation estimate for the test error is approximately 24.23

# We can repeat this procedure for increasingly complex polynomial fits.
# To automate this process, we use a for() loop which iteratively fits
# polynomial regressions for polynomials of order i=1 to i=10, computes the 
# associated cross-validation error, and stores it in the ith element of the 
# vector cv.error
# We begin by initializing the vector.

cv.error <- rep(0,10) # repeat 0 10 times

for(i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

# We can see a sharp drop in the estimated test MSE between the linear and 
# quadratic fits, but then no clear improvement from using higher order polynomials

################################################################################
# k-Fold Cross-Validation
################################################################################

# The cv.glm() function can also be used to implement k-fold CV. Below we use k=10
# on the Auto data set.
# We set a random seed and initialize a vector in which we will store the CV errors
# corresponding to the polynomial fits of orders one to ten.

set.seed(17)
cv.error.10 <- rep(0, 10)

for(i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}

cv.error.10

# Notice that the computation time is shorter than that of LOOCV
# We still see little evidence that using cubic or higher-order polynomial terms
# leads to lower test error than simply using a quadratic fit.

################################################################################
# The Bootstrap
################################################################################

# Estimating the Accuracy of a Statistic of Interest

# Performing a bootstrap analysis in R entails only two steps:
# First, we create a function that computes the statistic of interest.
# Second,, we use the boot() function, to perform a bootstrap by repeatedly
# sampling observations from the data set with replacement

# The Portfolio data set is simulated data of 100 pairs of returns. 
# To illustrate the use of the bootstrap on this data, we must first create 
# a function, alpha.fn(), which takes as input the (X, Y) data as well as a 
# vector indicating which observations should be used to estimated alpha.
# The function then outputs the estimate for alpha based on the selected observations

alpha.fn <- function(data, index){
  # this function returns an estimate of alpha
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

# this command estimates alpha based on all 100 observations

alpha.fn(Portfolio, 1:100)
# 0.5758

# this next command uses the sample() function to randomly select 100 observations
# from the range 1 to 100, with replacement. 
# This is equivalent to constructing a new bootstrap data set and recomputing
# alpha based on the new data set.

set.seed(7)
alpha.fn(Portfolio, sample(100,100, replace=TRUE))
# 0.5385

# We can implement a bootstrap analysis by performing this command many times,
# recording all of the corresponding estimates for alpha, and computing the 
# resulting standard deviation. 
# However, the boot() function automates this approach. 
# Below, we produce R=1000 bootstrap estimates for alpha.

boot(Portfolio, alpha.fn, R=1000)
# alpha=0.5758, bootstrap estimate for SE(alpha) = 0.0897


# Estimating the Accuracy of a Linear Regression Model

# The bootstrap approach can be used to assess the variability of the coefficient
# estimates and predictions from a statistical learning method.
# Here we use the bootstrap approach in order to assess the variability of the 
# estimates for B_0 and B_1, the intercept and slope terms for the linear
# regression model that uses horsepower to predict mpg in the Auto data set.

# We first create a function, boot.fn(), which takes in the  Auto data set
# as well as a set of indices for the observations, and returns the intercept
# and slope estimates for the linear regression model.
# We then apply this function to the full set of 392 observations in order to compute 
# the estimates of B_0 and B_1 on the entire data set using the usual linear regression
# coefficient estimate formulas. 

boot.fn <- function(data, index)
  coef(lm(mpg ~ horsepower, data=Auto, subset=index))

boot.fn(Auto, 1:392)
# Intercept: 39.93586, horsepower: -0.1578

# The boot.fn() function can also be used to create bootstrap estimates for the
# intercept and slope terms by randomly sampling from among the observations 
# with replacement.

set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
# Intercept: 40.34 horsepower: -0.1635

# Next we use the boot() function to compute the standard errors of 1000
# bootstrap estimates for the intercept and slope terms

boot(Auto, boot.fn, 1000)

# This indicates that the bootstrap estimate for SE(B_0) is 0.84
# SE(B_1) is 0.0073

summary(lm(mpg ~ horsepower, data=Auto))$coef
#SE(B_0): 0.717, SE(B_1): 0.0064

# these estimates are different from what we obtained from bootstrap method.
# Standard errors from lm() function depend on certain assumptions.

# The bootstrap assumption does not rely on any assumptions, and so it is likely 
# to give a more accurate estimate of the standard errors of B_0 and B_1 than the summary() function.

# Below we compute the bootstrap standard error estimates and the standard linear regression estimates
# that result from fitting the quadratic model to the data.

boot.fn <- function(data, index)
  coef(
    lm(mpg ~ horsepower + I(horsepower^2), data=Auto, subset=index)
  )

set.seed(1)
boot(Auto, boot.fn, 1000)

# SE(B_0): 2.03, SE(B_1): 0.032, SE(B_2): 0.0001

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef

# SE(B_0): 1.8, SE(B_1): 0.031, SE(B_2): 0.0001