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
mean(glm.preds == Default$default)

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
mean(glm.preds == Default[-train,]$default) # 0.9737

set.seed(334)
train <- sample(10000, 5000)
glm.fit2 <- glm(default ~ balance + income, family=binomial, data=Default, subset=train)

glm.probs <- predict(glm.fit2, Default[-train,], type="response")
glm.preds <- ifelse(glm.probs > 0.5, "Yes", "No")
mean(glm.preds == Default[-train,]$default)
# 97.2% 

set.seed(1000)
train <- sample(10000, 5000)
glm.fit3 <- glm(default ~ balance + income, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit3, Default[-train,], type="response")
glm.preds <- ifelse(glm.probs>0.5, "Yes", "No")
mean(glm.preds == Default[-train,]$default)
# 97.16%

# c.) Repeat the process in (b) three times, using three different splits of the
# observations into a training set and a validation set. 
# Comment on the results obtained.

# The validation changes but not significantly. This is because the Default
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
mean(glm.preds==Default[-train,]$default) # .9728

# Including a dummy variable does not lead to a reduction in test error rate

###############################################################################
# 6. We continue to consider the use of a logistic regression model to predict 
# the probability of default using income and balance on the Default data set.
# In particular, we will now compute estimates for the standard errors of the income
# and balance logistic regression coefficients in two different ways:
# (1) using the bootstrap
# (2) using the standard formula for computing the standard errors in the glm() function
# Do not forget to set a random seed before beginning your analysis.

# a.) Using the summary() and glm() functions, determine the estimated standard errors
# for the coefficients associated with income and balance in a multiple logistic 
# regression model that uses both predictors.


# b.) Write a function, boot.fn(), that takes as input the Default data set
# as well as an index of the observations, and that outputs the coefficient
# estimates for income and balance in the multiple logistic regression model.


# c.) Use the boot() function together with your boot.fn() function to estimate the
# standard errors of the logistic regression coefficients for income and balance.


# d.) Comment on the estimated standard errors obtained using the glm() function
# and using your bootstrap function.


################################################################################
# 7. The cv.glm() function can be used in order to compute the LOOCV test error estimate.
# Alternatively, one could compute those quantities using just the glm() and predict.glm()
# functions, and a for loop. 
# You will take this approach in order to compute the LOOCV error for a simple 
# logistic regression model on the Weekly data set.

# a.) Fit a logistic regression model that predicts Direction using Lag1 and Lag2


# b.) Fit a logistic regression model that predicts Direction using Lag1 and Lag2
# Using all but the first observation.


# c.) Use the model from (b) to predict the direction of the first observation.
# You can do this by predicting that the first observation will go up if 
# P(Direction="Up"|Lag1, Lag2) > 0.5. 
# Was this observation correctly classified?


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


# e.) Take the average of the n numbers obtained in (d) iv in order to obtain the
# LOOCV estimate for the test error. Comment on the results.



# 8. We will now perform cross-validation on a simulated data set.

# a.) Generate a simulated data set as follows:

set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

# In this data set, what is n and what is p?
# Write out the model used to generate the data in equation form

# b.) Create a scatter plot of X against Y. Comment on what you find.


# c.) Set a random seed, and then compute the LOOCV errors that result from fitting
# the following four models using least squares:

# i. Y = B_0 + B_1 X + e

# ii. Y = B_0 + B_1 X + B_2 X^2 + e

# iii. Y = B_0 + B_1 X + B_2 X^2 + B_3 X^3 + e

# iv. Y = B_0 + B_1 X + B_2 X^2 + B_3 X^3 + B_4 X^4 + e

# Note that you may find it helpful to use the data.frame() function to create
# a single data set containing bot X and Y.


# d.) Repeat (c) using another random seed, and report your results. Are your 
# results the same as what you got in (c) ? Why?


# e.) Which of the models in (c) had the smallest LOOCV error? 
# Is this what you expected? Explain your answer.


# f.) Comment on the statistical significance of the coefficient estimates that
# result from fitting each of the models in (c) using least squares. 
# Do these results agree with the conclusions drawn based on the cross-validation results?



################################################################################
# 9. We will now consider the Boston housing data set

# a.) Based on this data set, provide an estimate for the population mean of medv.
# Call this estimate mu_hat.


# b.) Provide an estimate of the standard error of mu_hat. Interpret this result
# Hint: We can compute the standard error of the sample mean by dividing the sample
# standard deviation by the square root of the number of observations.


# c.) Now estimate the standard error of mu_hat using the bootstrap.
# How does this compare to your answer from (b)?


# d.) Based on your bootstrap estimate from (c), provide a 95% confidence interval
# for the mean of medv. Compare it to the results obtained using t.test(Boston$medv)

# Hint: You can approximate a 95% CI using the formula [mu_hat -2SE(mu_hat), mu_hat + 2SE(mu_hat)].


# e.) Based on this data set, provide an estimate, mu_hat_med, for the median value
# of medv in the population.


# f.) We now would like to estimate the standard error of mu_hat_med.
# Unfortunately there is no simple formula for computing the standard error of the median.
# Instead, estimate the standard error of the median using the bootstrap.
# Comment on your findings.


# g.) Based on this data set, provide an estimate for the tenth percentile of medv
# in Boston census tracts. Call this quantity mu_hat_0.1
# Hint: You can use the quantile() function.


# h.) Use the bootstrap to estimate the standard error of mu_hat_0.1.
# Comment on your findings. 

