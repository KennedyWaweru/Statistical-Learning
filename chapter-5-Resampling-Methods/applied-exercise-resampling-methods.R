###############################################################################
# 5. In Chapter 4, we used logistic regression to predict the probability of
# default using income and balance on the Default data set. We will now estimate 
# the test error of this logistic regression model using the validation set
# approach. Do not forget to set a random seed before beginning your analysis.

# a.) Fit a logistic regression model that uses income and balance to predict default
library(ISLR2)
set.seed(1)
dim(Default)
# convert default column to numeric
glm.fit <- glm(default ~ balance + income, data=Default, family=binomial)
glm.probs <- predict(glm.fit, Default, type="response")
glm.preds <- ifelse(glm.probs > 0.5, "Yes", "No")
mean(glm.preds != Default$default) # Error rate
# 0.0263

# b.) Using the validation set approach, estimate the test error of this model.
# In order to do this you must perform the following steps:

# i. Split the sample set into a training set and a validation set.
# ii. Fit a multiple logistic regression model using only the training observations
# iii. Obtain a prediction of default status for each individual in the validation
# set by computing the posterior probability of default for that individual, and
# classifying the individual to the default category if the posterior probability
# is greater than 0.5
# iv. Compute the validation set error, which is the fraction of the observations
# in the validation set that are misclassified

# use 5000 observations as training
set.seed(2)
train <- sample(10000, 5000)

glm.fit1 <- glm(default ~ balance + income, family=binomial, data=Default, subset=train)
summary(glm.fit1)
glm.probs <- predict(glm.fit1, Default[-train,], type="response")
glm.preds <- ifelse(glm.probs>0.5, "Yes", "No")
mean(glm.preds != Default[-train,]$default) # test error rate
# 0.0238

set.seed(334)
train <- sample(10000, 5000)
glm.fit2 <- glm(default ~ balance + income, family=binomial, data=Default, subset=train)

glm.probs <- predict(glm.fit2, Default[-train,], type="response")
glm.preds <- ifelse(glm.probs > 0.5, "Yes", "No")
mean(glm.preds != Default[-train,]$default)
# 0.028

set.seed(1000)
train <- sample(10000, 5000)
glm.fit3 <- glm(default ~ balance + income, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit3, Default[-train,], type="response")
glm.preds <- ifelse(glm.probs>0.5, "Yes", "No")
mean(glm.preds != Default[-train,]$default)
# 0.0284

# c.) Repeat the process in (b) three times, using three different splits of the
# observations into a training set and a validation set. 
# Comment on the results obtained.

# The test error rate changes with change in the validation set chosen, but it does not change significantly. This is because the Default
# dataset is imbalanced and most observations did not default 96.7% were "No"

# d.) Now consider a logistic regression model that predicts the probability of
# default using income, balance, and a dummy variable for student. Estimate the
# test error for this model using the validation set approach. 
# Comment on whether or not including a dummy variable for student leads to a 
# reduction in the test error rate.
set.seed(123)
train <- sample(10000,5000)
glm.fit <- glm(default ~ income + balance + student, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit, Default[-train, ], type="response")
glm.preds <- ifelse(glm.probs>0.5, "Yes", "No")
mean(glm.preds!=Default[-train,]$default) # .0284

# Including a dummy variable does not lead to a reduction in test error rate

###############################################################################
# 6. We continue to consider the use of a logistic regression model to predict 
# the probability of default using income and balance on the Default data set.
# In particular, we will now compute estimates for the standard errors of the income
# and balance logistic regression coefficients in two different ways:
# (1) using the bootstrap
# (2) using the standard formula for computing the standard errors in the glm() function
# Do not forget to set a random seed before beginning your analysis.

set.seed(123)
# a.) Using the summary() and glm() functions, determine the estimated standard errors
# for the coefficients associated with income and balance in a multiple logistic 
# regression model that uses both predictors.

glm.fit <- glm(default ~ balance + income, data=Default, family=binomial)
summary(glm.fit)

# Estimated Standard Errors
# balance: 2.274e-04
# income: 4.985e-06


# b.) Write a function, boot.fn(), that takes as input the Default data set
# as well as an index of the observations, and that outputs the coefficient
# estimates for income and balance in the multiple logistic regression model.

boot.fn <- function(input, index){
  glm.fit <- glm(default ~ balance + income, data=input, subset=index, family=binomial)
  coefficients(glm.fit)[2:3]
}

boot.fn(Default, 1:2000)

# c.) Use the boot() function together with your boot.fn() function to estimate the
# standard errors of the logistic regression coefficients for income and balance.
library(boot)
boot(Default, boot.fn, 1000)

# bootstrap standard errors
# balance: 2.217214e-04,
# income: 4.729534e-06

# d.) Comment on the estimated standard errors obtained using the glm() function
# and using your bootstrap function.

# The standard errors obtained from glm() and those obtained from the bootstrap
# are different but not with a big margin. The glm() function makes some assumptions
# about the distribution of the target class while the bootstrap estimates 
# does not rely on any estimates and thus more likely to give accurate estimates

################################################################################
# 7. The cv.glm() function can be used in order to compute the LOOCV test error estimate.
# Alternatively, one could compute those quantities using just the glm() and predict.glm()
# functions, and a for loop. 
# You will take this approach in order to compute the LOOCV error for a simple 
# logistic regression model on the Weekly data set.

# a.) Fit a logistic regression model that predicts Direction using Lag1 and Lag2
glm.fit <-glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit, Weekly, type="response")
glm.preds <- ifelse(glm.probs>0.5,"Up","Down")
mean(glm.preds != Weekly$Direction)
# .444 error rate

# b.) Fit a logistic regression model that predicts Direction using Lag1 and Lag2
# Using all but the first observation.
train <- Weekly[-1,]
test <- Weekly[1,]
loocv_glm <- glm(Direction ~ Lag1+Lag2, data=train, family=binomial)
summary(loocv_glm)

# c.) Use the model from (b) to predict the direction of the first observation.
# You can do this by predicting that the first observation will go up if 
# P(Direction="Up"|Lag1, Lag2) > 0.5. 
# Was this observation correctly classified?
test.prob <- predict.glm(loocv_glm, test, type="response")
test.pred <- ifelse(test.prob > 0.5, "Up", "Down")
test.pred # Up

# This observation was misclassified 

# d.) Write a for loop from i = 1 to i = n, where n is the number of observations
# in the data set, that performs each of the following steps:

# i. Fit a logistic regression model using all but the ith observation to predict
# Direction using Lag1 and Lag2.
# ii. Compute the posterior probability of the market moving up for the ith observation
# iii. Use the posterior probability for the ith observation in order to predict
# whether or not the market moves up.
# iv. Determine whether or not an error was made in predicting the direction for 
# the ith observation. If an error was made, then indicate this as a 1, 
# and otherwise indicate it as a 0.

num_obs <- dim(Weekly)[1]
for(i in 1:num_obs){
  train <- Weekly[-i,]
  test <- Weekly[i,]
  
  glm.fit <- glm(Direction ~ Lag1+Lag2, data=train, family=binomial)
  test.prob <- predict(glm.fit, test, type="response")
  test.pred <- ifelse(test.prob>0.5,"Up","Down")
  pred_right[i] <- ifelse(test.pred == test$Direction, 0, 1)
  #print(pred_right)
}

# e.) Take the average of the n numbers obtained in (d) iv in order to obtain the
# LOOCV estimate for the test error. Comment on the results.

length(pred_right) # 1089
mean(pred_right) # .449954

# This method only got 45% test error rate

# 8. We will now perform cross-validation on a simulated data set.

# a.) Generate a simulated data set as follows:

set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

# In this data set, what is n and what is p?
# Write out the model used to generate the data in equation form

# n: number of observations = 100
# p: number of variables = 2 (x and y)

# Equation: Y = x + x*2 + e

# b.) Create a scatter plot of X against Y. Comment on what you find.

plot(x, y)

# there is a nonlinear r/ship between x and y

# c.) Set a random seed, and then compute the LOOCV errors that result from fitting
# the following four models using least squares:

# i. Y = B_0 + B_1 X + e

# ii. Y = B_0 + B_1 X + B_2 X^2 + e

# iii. Y = B_0 + B_1 X + B_2 X^2 + B_3 X^3 + e

# iv. Y = B_0 + B_1 X + B_2 X^2 + B_3 X^3 + B_4 X^4 + e

# Note that you may find it helpful to use the data.frame() function to create
# a single data set containing both X and Y.
library(boot)

# create a dataframe to contain the data for the cv.glm(data, model) function
df = data.frame(X=x, Y=y)

set.seed(123)
model_1 = glm(Y~X, data=df)
loocv_model_1 = cv.glm(df, model_1)
loocv_model_1$delta[1]
# model_1 loocv error: 7.28816

model_2 = glm(Y~X+I(X**2), data=df)
loocv_model_2 <- cv.glm(df, model_2)
loocv_model_2$delta[1]
# model_2 loocv error: 0.9374

model_3 = glm(Y~X+I(X**2)+I(X**3), data=df)
loocv_model_3 = cv.glm(df, model_3)
loocv_model_3$delta[1]
# model_3 loocv error: 0.9566

#model_3 = lm(y~x+I(x^2)+I(x^3))
model_4 = glm(Y~poly(X,4), data=df)
loocv_model_4 <- cv.glm(df, model_4)
loocv_model_4$delta[1]
# model_4 loocv error: 0.9539


# d.) Repeat (c) using another random seed, and report your results. Are your 
# results the same as what you got in (c) ? Why?

set.seed(456)
model_1 = glm(Y~X, data=df)
loocv_model_1 = cv.glm(df, model_1)
loocv_model_1$delta[1]
# model_1 loocv error: 7.28816

model_2 = glm(Y~X+I(X**2), data=df)
loocv_model_2 <- cv.glm(df, model_2)
loocv_model_2$delta[1]
# model_2 loocv error: 0.9374

model_3 = glm(Y~X+I(X**2)+I(X**3), data=df)
loocv_model_3 = cv.glm(df, model_3)
loocv_model_3$delta[1]
# model_3 loocv error: 0.9566

#model_3 = lm(y~x+I(x^2)+I(x^3))
model_4 = glm(Y~poly(X,4), data=df)
loocv_model_4 <- cv.glm(df, model_4)
loocv_model_4$delta[1]
# model_4 loocv error: 0.9539

# The error after changing the seed remains constant. 
# Comment: The LOOCV error does not change after changing the seed because 
# the method leaves only one observation out and changing the randomization 
# does not affect the error because the same observations will be used

# e.) Which of the models in (c) had the smallest LOOCV error? 
# Is this what you expected? Explain your answer.

# Model 2: Y = B_0 + B_1 X_1 + B_2 X_2^2 + e
# I expected this. From the scatter plot it is evident that the variables have a 
# quadratic relationship and the quadratic model has the lowers error.
# Higher polynomial produce higher errors since the curve is a simple curve
# indicating a lower polynomial term

# f.) Comment on the statistical significance of the coefficient estimates that
# result from fitting each of the models in (c) using least squares. 
# Do these results agree with the conclusions drawn based on the cross-validation results?
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)

# Comment: Only the first and second term coefficients are statistically significant
# for all models. This indicates that the variables have a quadratic relationship
# and higher polynomial orders do not provide a good fit for the model.
# According to the cross-validation method, model_2 has the lowest error 
# indicating that the model with two terms is most suitable, and thus the only
# significant coefficients are for the first and second terms.


################################################################################
# 9. We will now consider the Boston housing data set

library(ISLR2) # to obtain the Boston data set
help(Boston)

# a.) Based on this data set, provide an estimate for the population mean of medv.
# Call this estimate mu_hat.

mu_hat <- mean(Boston$medv)
mu_hat

# estimation of population mean: 22.5328

# b.) Provide an estimate of the standard error of mu_hat. Interpret this result
# Hint: We can compute the standard error of the sample mean by dividing the sample
# standard deviation by the square root of the number of observations.

sigma_square <- var(Boston$medv) # variance
std_dev <- sqrt(sigma_square) # standard deviation
num_obs <- length(Boston$medv)

std_error <- std_dev/sqrt(num_obs)
std_error

# standard error of mu_hat: 0.40886

# c.) Now estimate the standard error of mu_hat using the bootstrap.
# How does this compare to your answer from (b)?

boot.fn <- function(data, idx){
  to_use <- data$medv[idx]
  sigma_square <- var(to_use)
  std_dev <- sqrt(sigma_square)
  num_obs <- length(to_use)
  
  (std_err <- std_dev/sqrt(num_obs))
}

boot.fn(Boston, 1:100)

boot(Boston,boot.fn,1000)

# Bootstrap Estimate: 0.40886
# The bootstrap estimate of the standard error is equal to the standard error
# obtained using the formula above

# d.) Based on your bootstrap estimate from (c), provide a 95% confidence interval
# for the mean of medv. Compare it to the results obtained using t.test(Boston$medv)

# Hint: You can approximate a 95% CI using the formula [mu_hat -2SE(mu_hat), mu_hat + 2SE(mu_hat)].

lower_ci_bound = mu_hat - 2*(0.40886)
upper_ci_bound = mu_hat + 2*(0.40886)
lower_ci_bound
upper_ci_bound
# The 95% CI = [21.71509, 23.35053]

t.test(Boston$medv)
# The 95% CI = [21.72953, 23.33608]

# e.) Based on this data set, provide an estimate, mu_hat_med, for the median value
# of medv in the population.
mu_hat_med = median(Boston$medv)
mu_hat_med
# median value estimate = 21.2

# f.) We now would like to estimate the standard error of mu_hat_med.
# Unfortunately there is no simple formula for computing the standard error of the median.
# Instead, estimate the standard error of the median using the bootstrap.
# Comment on your findings.

boot.fn <- function(data, idx){
  to_use <- data$medv[idx]
  med_hat <- median(to_use)
  med_hat
}
boot.fn(Boston, sample(400,400))
boot(Boston, boot.fn, 1000)

# Standard error of the median estimate = 0.39014

# g.) Based on this data set, provide an estimate for the tenth percentile of medv
# in Boston census tracts. Call this quantity mu_hat_0.1
# Hint: You can use the quantile() function.

mu_hat_0.1 <- quantile(Boston$medv, .1) 
mu_hat_0.1
# estimate: 12.75

# h.) Use the bootstrap to estimate the standard error of mu_hat_0.1.
# Comment on your findings. 

boot.fn <- function(data, idx){
  to_use <- data$medv[idx]
  mu_hat_0.1 <- quantile(to_use, 0.1)
  mu_hat_0.1
}
boot.fn(Boston, sample(dim(Boston)[1], 400, replace=T))
# Estimate: 13.07
boot(Boston, boot.fn, 1000)

# standard error of estimate: 0.49426