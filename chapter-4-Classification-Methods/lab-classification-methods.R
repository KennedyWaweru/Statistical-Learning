library(ISLR2)

# The Stock Market Data
# The Smarket data consists of percentage returns for the S&P 500 stock index
# over 1250 days, from the beginning of 2001 until the end of 2005.
# For each date, the percentage returns for each of the five previous trading days Lag1 to Lag5 re recorded.
# Volume: number of shares traded on the previous day, in billions
# Today: the percentage return on the date in question
# Direction: Whether the market was Up or Down on this date

# The goal is to predict Direction (a qualitative response) using the other features
names(Smarket) # get the names of the available variables
head(Smarket) # peek the first 5 rows of the data set
dim(Smarket) # number of observations and number of variables
summary(Smarket)
# Direction Down: 602 days, Direction Up: 648 days. Good balance
pairs(Smarket)

# get the correlation matrix. 
# The cor function does not allow for qualitative variables, so except the Direction column
cor(Smarket[, 1:8])
# the correlations between the lag variables and today's returns are close to zero
# There seems to be little correlation between today's returns and previous day's returns
# The only substantial correlation is between Year and Volume

attach(Smarket)
plot(Volume)
# average number of shares traded daily increased through the years 2001-2005

################################################################################
# Logistic Regression
###############################################################################
# We fit a logistic regression model to predict Direction using Lags and Volume
# The glm() function can be used to fit many types of GLMs, including logistic regression
# We must pass the argument family=binomial to tell R to run a logistic regression model
# The logistic regression model assumes the response variable follows a binomial distribution

glm.fit <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
  data=Smarket, 
  family=binomial)
summary(glm.fit)
# the smallest p-value here is associated with Lag1. p-value: 0.145
# The negative coefficient for this predictor suggests that if the market had a positive return yesterday,
# then is is likely to go up today. 
# However, at a value of 0.15, the p-value is still relatively large, 
# and so there is no clear evidence or a real association between Lag1 and Direction

# The predict() function can be used to predict the probability that the market will go up, given values of the predictors.
# The type="response" option tells R to output probabilities of the form P(Y=1|X),
# as opposed to other information such as the logit.
# If no data set is supplied to the predict() function,
# then the probabilities are computed for the training data that was used to fit the logistic regression model.

glm.probs <- predict(glm.fit, type="response")
glm.probs[1:10]
contrasts(Direction)
Direction[1:10]

# in order to make a prediction as to whether the market will go up or down on a particular day,
# we convert these predicted probabilities into class labels, Up or Down.
glm.pred <- rep("Down", dim(Smarket)[1])
glm.pred[glm.probs > 0.5] = "Up"
# the above commands done using the ifelse command
glm.preds <- ifelse(glm.probs>0.5, "Up","Down")

# The table() function can be used to produce a confusion matrix
table(glm.pred, Direction)
# diagonals of the confusion matrix indicate correct predictions
# off-diagonals of the confusion matrix indicate incorrect predictions
mean(glm.pred==Direction) # get the accuracy score
# 0.5216: training accuracy

# to get a more realistic estimation of how the model will perform on unseen data
# we use the held out method
# To implement the hold out method, we create a vector corresponding to the observations from 2001 to 2004
# We will use this vector to create a held out data set of observations from 2005
train <- (Year<2005) # Boolean vector
tail(train)
Smarket.2005 <- Smarket[!train,] # exclude TRUE train values 
dim(Smarket.2005) # 252 observations, 9 variables
Direction.2005 <- Direction[!train]
length(Direction.2005) # 252

# We now fit a logistic regression model using only the subset of the observations
# that correspond to dates before 2005, using the subset argument.
# We then obtain predicted probabilities of the stock market going up for each of the days in the test set

glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
  data = Smarket,
  family = binomial,
  subset = train
)
glm.probs <- predict(glm.fits, Smarket.2005, type="response")
# Finally, we compare the results of our predictions and the actual values for the directions of 2005
glm.pred <- ifelse(glm.probs > 0.5, "Up","Down")
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) # test accuracy: 0.48
mean(glm.pred != Direction.2005) # test error: 0.52

# Perhaps removing some variables that appear not helpful in predicting Direction might produce a better model
# using predictors that have no r/ship with the response variable 
# tends to increase the variance without a corresponding decrease in bias
# removing such predictors may yield in model improvement.

# Below we fit the logistic regression model using just Lag1 and Lag2

glm.fits <- glm(
  Direction ~ Lag1 + Lag2,
  data = Smarket,
  family = binomial,
  subset = train
)
glm.probs <- predict(glm.fits, Smarket.2005, type="response")
glm.preds <- ifelse(glm.probs > 0.5, "Up", "Down")
table(glm.preds, Direction.2005)
mean(glm.preds==Direction.2005) # test accuracy: 0.56
mean(glm.preds != Direction.2005) # test error rate: 0.44
# Results have improved 

# Suppose that we want to predict the returns associated with particular values of Lag1 and Lag2
# In particular, we want to predict Direction on a day when Lag1=1.2 and Lag2=1.1
# and on a day when Lag1=1.5 and Lag2=-0.8

predict(glm.fits,
        newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)),
        type="response")

#################################################################################
# Linear Discriminant Analysis
####################################################################################
# Now we perform LDA on the Smarket data
# use the lda() function, which is part of the MASS library
# syntax of lda() is similar to that of lm() and glm() except for the family argument
# we fit the model using only the observations before 2005

library(MASS)
# create subset of training data
train <- Smarket$Year < 2005
lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit
# prior probability of Down: 0.49 
# prior probability of Up: 0.508 -> 50.8% of observations correspond to days where market went up
# The group means are the average (mu) of each predictor within each class
# The coefficients of linear discriminant output provides the linear combination of Lag1 and Lag2
# that are used to form the LDA decision rule.

# The plot() function produces plots of the linear discriminants
# obtained by computing the linear combinations of the predictors for each training observations 
plot(lda.fit)

# The predict() function returns a list with three elements
# The first element, class, contains LDA's prredictions about the movement of the market
# The second element, posterior, is a matrix whose kth column contains the posterior probability
# that the corresponding observation belongs to the kth class
# The third element, x, contains the linear descriminants

Smarket.2005 <- Smarket[!train, ]
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
# class, posterior, x
lda.class <- lda.pred$class
table(lda.class, Smarket.2005$Direction)


mean(lda.class==Smarket.2005$Direction)
# accuracy score: 0.56

# applying a 50% threshold to the posterior probabilities
# allows us to recreate the predictions contained in lda.pred$class
sum(lda.pred$posterior[, 1] >= 0.5)
# 70
sum(lda.pred$posterior[, 1] < 0.5)
# 182
# notice that the posterior probability output by the model corresponds to the probability that the market will decrease

lda.pred$posterior[1:20, 1]
lda.class[1:20]

# suppose we wanted to use a posterior probability threshold other than 50% in order to make predictions,
sum(lda.pred$posterior[, 1] > .9)
# 0 
max(lda.pred$posterior[,1])
# maximum probability was 0.520 for year 2005

##########################################################################################
# Quadratic Discriminant Analysis
###################################################################################
# We will now fit a QDA model to the Smarket data
# QDA is implemented using the qda() function which i part of the MASS library
# qda() syntax is identical to that of lda()

qda.fit <- qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit

# The output contains the group means
# output does not contain the coefficients of the discriminants

# The predict() function works in exactly the same fashion as for LDA
qda.class <- predict(qda.fit, Smarket.2005)
names(qda.class)
# class, posterior
table(qda.class$class, Smarket.2005$Direction)

mean(qda.class$class == Smarket.2005$Direction)
# accuracy score: 0.60
# the QDA predictions are accurate almost 60% of the time
# The level of accuracy is quit impressive for stock market data
# this suggests that the quadratic form assumed by QDA 
# may capture the true r/ship more accurately than the LDA and logistic regression

#################################################################################
# Naive Bayes
###################################################################################
# We fit a Naive Bayes model to the Smarket data.
# Naive Bayes is implemented in R using the naiveBayes() function, which is part of e1071 library
# syntax is identical to that of lda() and qda()
# by default this implementation of the naive Bayes classifier models each quantitative feature
# using a Gaussian distribution. 
# However, a kernel density method can also be used to estimate the distributions.

library(e1071)
nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
nb.fit

# The output contains the estimated mean and standard deviation for each variable in each class.
# for instance, the mean for Lag1 is 0.0428 for Direction=Down,
# and standard deviation is 1.23 
# we can easily verify the results
mean(Smarket$Lag1[train][Smarket$Direction[train]=="Down"])
# 0.04279
sd(Smarket$Lag1[train][Smarket$Direction[train]=="Down"])
# 1.227

# The predict() function is straightforward
nb.pred <- predict(nb.fit, Smarket.2005)
table(nb.pred, Smarket.2005$Direction)
mean(nb.pred==Smarket.2005$Direction)
# 0.591

# Naive Bayes performs very well on this data, with accurate predictions over 59% of the time
# This is slightly worse than QDA but better than LDA and logistic regression

# The predict() function can also generate estimates of the probability that each observation belongs to a particular class
nb.probs <- predict(nb.fit, Smarket.2005, type="raw")
nb.probs[1:5, ]

##########################################################################################
# K-Nearest Neighbor
################################################################################
# now we fit the KNN on the Smarket data
# we use the knn() function, which is part of the class library
library(ISLR2) # for Smarket data set
library(class)
?knn
# nn() function is different from other fitting functions we have encountered thus far
# Rather than a two-step approach in which we first fir the model and then we use the model to make predictions,
# knn() forms predictions using a single command
# The function requires four inputs:
# 1. A matrix containing the predictors associated with the training data, labeled train.X below
# 2. A matrix containing the predictors associated with the data for which we wish to make predictions, labeled test.X
# 3. A vector containing the class labels for the training observations, labeled train.Direction
# 4. A value for K, the number of nearest neighbors to be used by the classifier

# We use cbind() "column bind" to bind the Lag1 and Lag2 variables together into two matrices,
# one for the training set and the other for the test set
train <- Smarket$Year < 2005
Direction.2005 <- Smarket$Direction[!train]
train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train, ]
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train, ]
train.Directions <- Smarket$Direction[train]

# Now the knn() function can be used to predict the market's movement for the dates in 2005.
# Set a random seed for reproducibility
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Directions, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
# 0.5 
# when K=1 results are not very good, since only 50% of the observations are correctly predicted

knn.pred <- knn(train.X, test.X, train.Directions, k=3)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)
# 0.536
# results have improved slightly.
# KNN does not provide good predictions for the Smarket data but it does often provide impressive results

# We will apply the KNN approach to the Caravan data set.
# This data set includes 85 predictors that measure demographic characteristics for 5822 individuals.
# The response variable is Purchase,
# which indicates whether or not a given individual purchases a caravan insurance policy.
# In this data set, only 6% of people purchased caravan insurance.

dim(Caravan)
attach(Caravan)
summary(Purchase)
# no: 5474
# Yes: 348

# Because KNN classifier predicts the class of a given test observation
# by identifying the observations that are nearest to it, the scale of the variables matters.
# Variables that are on a large scale will have a much larger effect 
# on the distance between the observations, and hence on the KNN classifier,
# than variables that are on a small scale.
# A good way to handle this problem is to standardize the data
# so that all variables are given a mean of zero and a standard deviation of one.
# Then all variables will be on a comparable scale.
# The scale() function does just this
# In standardizing the data, we exclude column 86, because that is the qualitative Purchase variable

standardized.X <- scale(Caravan[, -86])
var(Caravan[, 1]) # 165.038
var(Caravan[, 2]) # 0.1647
var(standardized.X[,1]) # 1
var(standardized.X[,2]) # 1

# split the observations into a test set, containing the first 1,000 observations
# and a training set, containing the remaining observations.

test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
# error rate: 0.118
mean(test.Y != 'No')
# 0.059
# The KNN error rate on the 1000 test observations is just under 12%
# This may appear go be fairly good. However, since only 6% of Y is yes
# we could get the error rate down to 6% by always predicting No

# The company would like to try to sell insurance only to customers who are likely to buy it.
# So the overall error rate is not of interest.
# Instead, the fraction of individuals that are correctly predicted to buy insurance is of interest.

table(knn.pred, test.Y)
9/(68+9) 
# success rate is 11.7 when k=1

# Using K=3, the success rate increases to 19%
knn.pred <- knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
5/(21+5)
# 19.23%

# Using K=5 the rate is 26.7%
knn.pred <- knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
4/(11+4)
# 0.267

# As comparison we also fit a logistic regression model to the data.
# If we use 0.5 as the threshold for the classifier, then we have a problem:
# only seven of the test observations are predicted to purchase insurance.
# all these seven predicted observations are wrong

# if instead we predict a purchase any time the predicted probability exceeds 0.25, 
# we get better results: we predict that 33 people will purchase insurance
# And we are correct for about 33% of these people.

glm.fits <- glm(Purchase ~ ., data=Caravan, family=binomial, subset=-test)

glm.probs <- predict(glm.fits, Caravan[test, ], type="response")
#glm.pred <- rep('No', 1000)
glm.pred <- ifelse(glm.probs > .5, "Yes", "No")
table(glm.pred, test.Y)

glm.pred <- ifelse(glm.probs > .25, "Yes", "No")
table(glm.pred, test.Y)
11/(22+11)
# 0.333

##################################################################################
# Poisson Regression
###################################################################################
# We fit a Poisson regression model to the Bikeshare data set,
# which measures the number of bike rentals (bikers) per hour in Washington, DC.
# The data set is part of the ISLR2 library
# Poisson regression: done with the glm() function
# argument family has to be set to Poisson
library(ISLR2)
attach(Bikeshare)
dim(Bikeshare)
# 8645 rows, 15 columns
names(Bikeshare)

# begin by fitting a least squares regression model to the data
mod.lm <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit,
  data=Bikeshare
)
summary(mod.lm)
# the first level of hr (0) and mnth (Jan) are treated as baseline values,
# so no coefficient estimates are provided for them:
# their coefficient is zero, and all other levels are measured relative to the baselines
# The Feb coefficient of 6.845 signifies that,
# holding all other variables constant, there are on average about 7 more riders in Feb than in Jan

### Let's use different coding for the two qualitative variables
contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)
mod.lm2 <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit,
  data=Bikeshare
)
summary(mod.lm2)

# in mod.lm2 a coefficient for all months is reported except for the last month 12
# a coefficient of all hours is reported except for the last hour 24

# In mod.lm2, the coefficient for the last level of mnth is not zero:
# it is the negative of the sum of the coefficient estimates for all other levels

# the coefficient for the last level of hour is the negative sum of the coefficient
# estimates for all of the other levels

# This means that the coefficients of hr and mnth in mod.lm2 will always sum to zero,
# and can be interpreted as the difference from the mean level
# Example: the coefficient for January of -46.087 indicates that
# holding all other variables constant, 
# there are typically 46 fewer rides in January relative to yearly average

# The choice of coding does not matter, provided that we interpret the model output correctly

sum((predict(mod.lm)- predict(mod.lm2))^2)
# virtually zero, the sum of differences squared
# predictions from both models are the same
all.equal(predict(mod.lm),predict(mod.lm2))
# TRUE 

# to get the coefficient of the missing level in mod.lm2 we use the negative sum of other coefficients
coef.months <- c(coef(mod.lm2)[2:12], -sum(coef(mod.lm2)[2:12]))
plot(coef.months, xlab="Month", ylab="Coefficient", xaxt="n", col="blue", pch=19, type="o")
# manually label the x-axis with the names of the months
axis(side=1, at=1:12, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))

# create a coefficient plot for hours
coef.hrs <- c(coef(mod.lm2)[13:35], -sum(coef(mod.lm2)[13:35]))
plot(coef.hrs, xlab="Hour", ylab="Coefficient", col="blue", pch=19, type="o")

# Now we fit a Poisson Regression model
mod.pois <- glm(
  bikers ~ mnth + hr + workingday + temp + weathersit,
  data=Bikeshare, family=poisson
)
summary(mod.pois)

# plot coefficients associated with month
coef.months <- c(coef(mod.pois)[2:12], -sum(coef(mod.pois)[2:12]))
plot(coef.months, xlab="Month", ylab="Coefficient",
     main="Month Coefficients for Poisson regression", col="blue", pch=19, type="o")
# manually label the x-axis with the names of the months
axis(side=1, at=1:12, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))

# coefficients for the hour variable
coef.hrs <- c(coef(mod.pois)[13:35], -sum(coef(mod.pois)[13:35]))
plot(coef.hrs, xlab="Hour", ylab="Coefficient",
     main="Hour Coefficients for Poisson Regression", col="blue", pch=19, type="o")

# use the predict variable with type="response"

plot(predict(mod.lm), predict(mod.pois, type="response"))
abline(0, 1, col=2, lwd=3)

# predictions of linear model are correlated with those of Poisson regression model
# predictions for Poisson regression are positive