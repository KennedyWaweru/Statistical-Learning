################################################################################
# This question should be answered using the Weekly data set, part of ISLR2 
# This data is similar in nature to the Smarket data
# except it contains 1089 weekly returns for 21 years,
# from the beginning of 1990 to the end of 2010

# a.) Produce some numerical and graphical summaries of the Weekly data.
# Do there appear to be any patterns?

library(ISLR2)
dim(Weekly)
head(Weekly)
summary(Weekly) # Direction is the only qualitative variable
pairs(Weekly[, -dim(Weekly)[2]])
cor(Weekly[, -dim(Weekly)[2]])

# Year and volume have a positive linear relationship 
# Volume of securities traded has increased steadily over the period from 1990 to 2010
# lagged returns and today's returns have very low correlation

# b.) Use the full data set to perform a logistic regression with Direction
# as the response and the five lag variables plus Volume as predictors
# use the summary function to print the results. Do any of the predictors
# appear to be statistically significant? If so, which ones?
glm.fit <- glm(
  Direction ~ .,
  data = Weekly,
  family=binomial
)
summary(glm.fit)
# none of the variables are statistically significant in predicting Direction

# c. ) Compute the confusion matrix and overall fraction of correct predictions
# Explain what the confusion matrix is telling you about the types of mistakes
glm.pred <- predict(glm.fit, Weekly, type="response")
glm.pred
glm.class <- ifelse(glm.pred > 0.5, 'Up', 'Down')
glm.class[1:10]
table(Weekly$Direction, glm.class)
# logistic regression does not make any mistakes
mean(Weekly$Direction==glm.class)
# accuracy of 100%

# d.) Now fit the logistic regression model using a training data
# period from 1990 to 2008, with Lag2 as the only predictor. 
# Compute the confusion matrix and the overall fraction of correct predictions 
# for the held out data (i.e data from 2009 to 2010)
train <- Weekly$Year < 2009
head(train)

glm.fit <- glm(
  Direction ~ Lag2,
  family=binomial,
  data=Weekly,
  subset=train
)
summary(glm.fit)
# lag2 is statistically significant with a p-value of 0.04
glm.probs <- predict(glm.fit, Weekly[!train, ], type='response')
glm.class <- ifelse(glm.probs > 0.5, 'Up', 'Down')
table(Weekly[!train,]$Direction, glm.class)
mean(Weekly[!train,]$Direction == glm.class)
# 62.5 % accuracy

# e.) Repeat (d) using LDA
library(MASS)
train <- Weekly$Year < 2009
lda.fit <- lda(
  Direction ~ Lag2, 
  data=Weekly,
  subset=train
)
lda.fit
# prior probability of Down: 0.45, Up: 0.55
lda.pred <- predict(lda.fit, Weekly[!train,], type="response")
table(lda.pred$class, Weekly[!train,]$Direction)
mean(lda.pred$class == Weekly[!train,]$Direction)
# lda accuracy: 62.5 %

# f.) Repeat (d) using QDA
qda.fit <- qda(
  Direction ~ Lag2,
  data=Weekly,
  subset=train
)
qda.fit

qda.pred <- predict(qda.fit, Weekly[!train, ], type="response")
table(qda.pred$class, Weekly[!train,]$Direction)
mean(qda.pred$class == Weekly[!train,]$Direction)
# qda accuracy: 58.65 %

# g.) Repeat (d) using KNN with K=1
library(class)
train.X <- Weekly[train, -dim(Weekly)[2]]
head(train.X)
train.Y <- Weekly[train, ]$Direction
test.X <- Weekly[!train, -dim(Weekly)[2]]
test.Y <- Weekly$Direction[!train]
knn.pred <- knn(train.X, test.X, train.Y, k=1)
table(knn.pred, test.Y)
mean(knn.pred==test.Y)
# knn accuracy = 79.8 %

# h.) Repeat (d) using naive Bayes
library(e1071)
nb.fit <- naiveBayes(
  Direction ~ Lag2,
  data=Weekly,
  subset=train
)

nb.fit
nb.pred <- predict(nb.fit, Weekly[!train,], type="class")
table(nb.pred, test.Y)
mean(nb.pred==test.Y)
# naive bayes accuracy: 58.65 %

# i.) Which of these methods appears to provide the best results on this data?
# the knn classifier provides the highest accuracy of 79%

# j.) Experiment with different combinations of predictors, including possible 
# transformations and interactions, for each of the methods.
# Report the variables, method, and associated confusion matrix 
# that appears to provide the best results on the held out data.
# Note that you should also experiment with values of K in the KNN classifier.

### TO-DO;

################################################################################
# 14. In this problem, you will develop a model to predict whether a given car
# gets high or low gas mileage based on the Auto data set.

# a.) Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median, and a 0 if mpg contains a value below its median.
head(Auto)
mpg_median <- median(Auto$mpg)
# creating a variable
Auto$mpg01 <- ifelse(Auto$mpg > mpg_median, 1, 0)
head(Auto)
# convert created column to qualitative variable
Auto$mpg01 <- as.factor(Auto$mpg01)
# b.) Explore the data graphically in order to investigate the association
# between mpg01 and the other features. 
# Which of the other features seem most likely to be useful in predicting mpg01?
# Scatterplots and boxplots may be useful tools to answer this question.

pairs(Auto[, -(dim(Auto)[2] - 1)])
plot(Auto$mpg01, Auto$cylinders)
plot(Auto$mpg01, Auto$weight)
plot(Auto$mpg01, Auto$acceleration)
#plot(Auto$mpg01, Auto$origin)
plot(Auto$mpg01, Auto$horsepower)
plot(Auto$mpg01, Auto$displacement)
plot(Auto$mpg01, Auto$year)

# c.) Split the data into a training set and a test set.
dim(Auto)
# use 80% of the observations as train set
train_split <- 0.8 * dim(Auto)[1]
train_idx <- 1:train_split
train <- Auto[train_idx,]
train.Y <- Auto[train_idx, ]$mpg01
test <- Auto[-train_idx, ]
test.X <- Auto[-train_idx, -(dim(Auto)[2]-1)]
test.Y <- Auto[-train_idx, ]$mpg01
length(test.Y)
dim(train)


# d.) Perform LDA on the training data in order to predict mpg01 using the variables
# that seem most associated with mpg01. 
# Report the test error of the model obtained
library(MASS)
lda.fit <- lda(
  mpg01 ~ displacement + horsepower + weight +  acceleration + year,
  data = train
  )
lda.fit

lda.pred <- predict(lda.fit, test)
table(lda.pred$class, test.Y)
mean(lda.pred$class == test.Y)
# test accuracy = 92.41%
# adding origin as a predictor leads to a 1% improvement in accuracy

# e.) Perform QDA on the training data in order to predict mpg01
# using the variables that seem most associated with mpg01 in (b) above.
# What is the test error of the model obtained?

qda.fit <- qda(
  mpg01 ~ displacement + horsepower + weight + acceleration + year,
  data = train
)
qda.fit

qda.pred <- predict(qda.fit, test)
table(qda.pred$class, test.Y)
mean(qda.pred$class == test.Y)
# qda accuracy = 0.899 89.97 %

# f.) Perform Logistic Regression on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in (b)
# What is the test error of the model obtained?
glm.fit <- glm(
  mpg01 ~ displacement+horsepower+weight+acceleration+year,
  data=train,
  family=binomial
)
summary(glm.fit)
# year is the only significant predictor

glm.probs <- predict(glm.fit, test.X, type="response")
glm.class <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.class, test.Y)
mean(glm.class == test.Y)
# logistic regression accuracy: 94.94 %

# g.) Perform naive Bayes on the training data in order to predict mpg01 
# using the variables that seemed most associated with mpg01 in (b) above
# Report the test error of the model obtained
library(e1071)
nb.fit <- naiveBayes(
  mpg01 ~ displacement+horsepower+weight+acceleration+year,
  data=train
)
nb.fit

nb.pred <- predict(nb.fit, test.X)
table(nb.pred, test.Y)
mean(nb.pred == test.Y)
# naive bayes accuracy: 91.14 %

# h.) Perform KNN on the training data, with several values of K, 
# in order to predict mpg01. Use only the variables that seemed associated wit mpg01
# Report the test errors you obtain.
# Which value of K seems to perform the best on this data set?
library(class)
train.X <- train[, -c(1,2,8,9,10)]
head(train.X)
test.X <- test[, -c(1,2,8,9,10)]
head(test.X)
knn.pred <- knn(
  train.X, test.X, train.Y, k=1
)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
# knn: k=1 accuracy = 78.48%

knn.pred <- knn(
  train.X, test.X, train.Y, k=2
)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
# knn: =2 accuracy = 0.81

knn.pred <- knn(
  train.X, test.X, train.Y, k=3
)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
# knn k=3 accuracy=74.68

knn.pred <- knn(
  train.X, test.X, train.Y, k=4
)
table(knn.pred, test.Y)
mean(knn.pred == test.Y)
# knn k=4 accuracy=74.68

# best K = k=2 with 81% accuracy

################################################################################
# 15. This problem involves writing functions

# a.) Write a function, Power(), that prints out the result of raising 2 to the 3rd power.
# In other words the function should compute 2^3 and print out the results

Power <- function(){
  result <- 2^3
  return(result) 
} 
Power()

# b.) Create a new function, Power2(), that allows you to pass any two numbers
# x and a, and prints out the value of x^a.

Power2 <- function(x, a){
  result <- x^a
  return(result)
}

Power2(3,8)
# 6561

# c.) Using the Power2() function that you just wrote, compute 10^3, 8^17 and 131^3
Power2(10,3) # 1000
Power2(8,17) # 2251799813685248
formatC(Power2(8,17), format="fg") # get integer representation
Power2(131,3) # 2248091

# e.) use the function to create a plot of f(x)=x^2
# The x-axis should display a range of integers from 1 to 10,
# and the y-axis should display x^2.
# Label the axes appropriately and use an appropriate title for the figure

x <- 1:10
y <- c(Power2(1,2),Power2(2,2),Power2(3,2),Power2(4,2),Power2(5,2),Power2(6,2),Power2(7,2),Power2(8,2),Power2(9,2),Power2(10,2))
plot(x, y,
     xlab="Number", ylab="Square of Number", main="Square of x against x"
     )

# f.) Create a function, PlotPower(), that allows you to create a plot
# of x against x^a for a fixed a and for a range of values of x
# Example: if you call PlotPower(1:10, 3) then a plot should be created
# with the x-axis taking on values 1,2,...,10, and a y-axis taking on values on 1^3, 2^3,...,10^3

PlotPower <- function(x, a){
  # x is a sequence
  # a is a real number
  x_vals <- x
  y_vals <- c()
  for(num in x){
    y_vals <- c(y_vals, num^a)
  }
  
  y_title <- paste("Number to power", a)
  plot_title <- paste0("Exponents of ",x[1],":",x[length(x)]," Raised to ",a)
  cool_plot <- plot(
    x=x_vals,
    y=y_vals,
    xlab="Number",
    ylab=y_title,
    main=plot_title
  )
  return(plot_title)
}

PlotPower(1:10,3)

####################################################################################
# 16. Using the Boston data set, fit classification models in order to predict
# whether a given census tract has a crime rate above or below the median.
# Explore logistic regression, LDA, naive Bayes, and KNN models
# using various subsets of the predictors.
# Describe your findings
################################################################################
head(Boston)
?Boston
# crim column is the per capita crime rate by town; our variable of interest
crim_median <- median(Boston$crim)
Boston$crim01 <- ifelse(Boston$crim > crim_median, 1, 0)
Boston$crim01 <- as.factor(Boston$crim01)
head(Boston)
summary(Boston)

# split the data into training and test sets
dim(Boston)
train_size <- 0.75 * dim(Boston)[1]
train_idx <- 1:train_size
train <- Boston[train_idx, ]
test <- Boston[-train_idx, ]

dim(train)[1] + dim(test)[1] == dim(Boston)[1]
# TRUE 
train.Y <- train$crim01
test.Y <- test$crim01

###############################################################################
# Logistic Regression
###############################################################################
glm.fit <- glm(
  crim01 ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,
  data=train,
  family=binomial
)
summary(glm.fit)
# significant predictors: nox, dis, rad, ptratio, medv

glm.pred <- predict(glm.fit, test, type="response")
glm.class <- ifelse(glm.pred > 0.5, 1, 0)
table(glm.class, test.Y)
mean(glm.class == test.Y)
# logistic regression accuracy: 92.13%

glm.fit <- glm(
  crim01 ~ nox+dis+rad+ptratio+medv,
  data=train,
  family=binomial
)
summary(glm.fit)

glm.probs <- predict(glm.fit, test, type="response")
glm.class <- ifelse(glm.probs > 0.5, 1, 0)
table(glm.class, test.Y)
mean(glm.class==test.Y)
# accuracy = 88.98 %

###############################################################################
# LDA
###############################################################################
lda.fit <- lda(
  crim01 ~ nox+dis+rad+ptratio+medv,
  train
)

lda.pred <- predict(lda.fit, test)
table(lda.pred$class, test.Y)
mean(lda.pred$class==test.Y)
# accuracy = 88.18

lda.full <- lda(
  crim01 ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,
  train
)

lda.pred <- predict(lda.full, test)
table(lda.pred$class, test.Y)
mean(lda.pred$class==test.Y)
# accuracy = 88.98%

##############################################################################
# naive Bayes
###############################################################################
nb.full <- naiveBayes(
  crim01 ~ zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black+lstat+medv,
  train
)
nb.pred <- predict(nb.full, test)
table(nb.pred,test.Y)
mean(nb.pred==test.Y)
# naive bayes accuracy: 88.19%

nb.fit <- naiveBayes(
  crim01 ~ nox+dis+rad+ptratio+medv,
  train
)

nb.pred <- predict(nb.fit, test) 
table(nb.pred, test.Y) 
mean(nb.pred==test.Y)
# naive bayes subset predictors accuracy: 88.19%

###############################################################################
# KNN
###############################################################################
head(train)
train.X <- train[, -c(1,dim(train)[2])]
head(train.X)
train.Y <- train$crim01 
test.X <- test[, -c(1,dim(train)[2])]
head(test.X)

knn.pred <- knn(
  train.X, test.X, train.Y, k=1
)

table(knn.pred, test.Y)
mean(knn.pred==test.Y)
# knn k=1 accuracy=93.7%

knn.pred <- knn(
  train.X, test.X, train.Y, k=2
)

table(knn.pred, test.Y)
mean(knn.pred==test.Y)
# 93.7%

# Best model: KNN with k=1 has best test accuracy


train.X <- train[, -c(1,2,3,4,6,7,8,10,12,13,15)]
head(train.X)
train.Y <- train$crim01 
test.X <- test[, -c(1,2,3,4,6,7,8,10,12,13,15)]
head(test.X)

knn.pred <- knn(
  train.X, test.X, train.Y, k=1
)

table(knn.pred, test.Y)
mean(knn.pred==test.Y)
# test set accuracy with subset of predictors: 92.91

# Best model: KNN with all other predictors 