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