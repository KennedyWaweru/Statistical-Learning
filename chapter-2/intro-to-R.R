# Basic Commands 

# to run a function called funcname, we type funcname(input1, input2) : input1, input2 are arguments

# create a vector
x <- c(1, 2, 2, 5)
x 

# check length of vector
length(x)

# add two sets of numbers together: x and y should be of same length
y <- c(3,4,7,6)
length(y)

# vector of different length
z <- c(4,5,6)
length(z)

# add vectors of same length
x + y

# add vectors of different length
x + z # produces warning message

# The ls() function allows us to look at a list of all of the objects, such as data and functions in our env
# The rm() function can be used to delete any of the objects we do not need

ls()
# output: "x" "y" "z"

rm(x,y)

ls()
# output: "z"

# remove all possible objects at once
rm(list=ls())

ls()
# output: character(0)

# The matrix() function can be used to create a matrix of numbers.
# To learn more about a function use a ? before the func name
?matrix
# output: produces the help file for the function

x <- matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x

# you can also omit the argument names
x <- matrix(c(1,2,3,4),2,2)
x

# if argument names are not specified R will assume that the function arguments 
# are passed into the function in the same order as specified in the function's help file.

matrix(c(1,2,3,4),2,2, byrow=TRUE)

# The sqrt() function returns the square root of each element of a vector or matrix.
sqrt(c(25,49,81))
# The x^2 command raises each element in x to the power 2;
c(5,6,7,8)^2
# any powers are possible, including fractions and negative powers
c(25,49,81)^(1/2)


# The rnorm() function generates a vector of random normal variables,
# the first argument n the sample size
rnorm(n=10) # by default rnorm() creates a standard normal random variable with mean=0, sd=1
rnorm(n=10)
# each time you call the rnorm() function we get a different answer

x <- rnorm(50)
y <- x + rnorm(50, mean=50, sd=.1)
cor(x,y)  # use cor() to compute the correlation between the vectors

# if we want our code to reproduce the exact same set of random numbers, we use the set.seed() function

set.seed(1303) # set.seed() takes an arbitrary integer argument
rnorm(50)

# set.seed() is used to ensure reproducibility by any user

# The mean() and var() are used to compute the mean and variance of vector of numbers
set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y)) # 0.856
sd(y) # 0.856


#### Graphics

# The plot() function is the primary way to plot data in R.
# plot(x,y) produces a scatterplot of the numbers in x versus the numbers in y

x <- rnorm(100)
y <- rnorm(100)
plot(x,y)

# There are many additional options that can be passed in to the plot()
?plot

plot(x,y,xlab="the x-axis",ylab='the y-axis',main="Plot of X vs Y")

# we will often want to save the output of an R plot.
# The command to use to do this depends on the file type we would like to create.
# to create a pdf use the pdf() function
# to create a jpeg use the jpeg() function

jpeg("Figure.jpeg")
plot(x,y,xlab="the x-axis",ylab='the y-axis',main="Plot of X vs Y",col="green")
dev.off()

# dev.off() indicates to R that we are done creating the plot and close the graphical device

# The function seq() can be used to create a sequence of numbers.
# seq(a,b) makes a vector of integers between a and b

x <- seq(1,10)
x

y <- seq(0,1,length=10)
y
# output: sequence of 0 numbers that are equally spaced between 0 and 1

x <- 3:11 # a shorthand for writing seq(3,11)
x

x <- seq(-pi, pi, length=50)
x

### Complicated Plots
# the contour() function produces a contour plot in order to represent three-dimensional data;
# it is a topographical map. It takes 3 arguments:
# 1. A vector of the x values (first dimension)
# 2. A vector of the y values (second dimension)
# 3. A matrix whose elements correspond to the z value (third dimension) for each pair of (x,y) coordinates

y <- x
f <- outer(x,y, function(x,y) cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=TRUE)
fa <- (f-t(f))/2
contour(x,y,fa,nlevels=15)

# the image() function works the same way as contour(), except it produces a color-coded plot
# whose color values depend on the z dimension
# this is a heatmap, and is sometimes used to plot temperature in weather forecasts.
image(x,y,fa)

# persp() can be used to produce a three-dimensional plot. 
# Arguments theta and phi control the angles at which the plot is viewed
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

###### Indexing Data
# We often wish to examine part of a set of data. Suppose our data is stored in matrix A

A <- matrix(c(1:16), 4,4)
A

A[2,3] # select element on second row and third column

# we can also select multiple rows and columns at a time, by providing vectors as indices
A[c(1,3), c(2,4)] # selects elements corresponding to first and third row, second and fourth column
A[1:3, 2:4] # selects elements from first to third row and second to fourth column
A[1:2, ] # selects elements in first and second row and all columns
A[, 1:2] # selects elements in first and second columns all rows
A[1,] # all elements in the first row

# use of negative sign (-) in the index
# tells R to keep all rows or columns except those indicated in the index
A[-c(1), -c(1)] # exclude all elements in first row and first column
A[-c(1,3), ] # exclude all elements from first and third rows
A[-c(1,3), -c(1,3,4)] # only retain the second and fourth rows and second column

# The dim() function outputs the number of rows and columns of a given matrix
dim(A)

#### Loading Data

# For most analyses, the first step involves importing a data set into R
# The read.table() functioon is one of the primary ways to do this.
# The write.table() functioon can be used to export the data
# Before attempting to load a data set,
# make sure that R knows to search for the data in the proper directory

# begin by loading in the Auto data set.
# we load the data from a text file, Auto.data
Auto <- read.table('../Datasets/Auto.data') # load the Auto.data text file into a data frame
View(Auto) # view the data in a spreadsheet-like window
head(Auto) # view the first few rows of the data

# this data set has not been loaded correctly,
# because R has assumed that the variable names are part of the data
# and so has included them in the first row.
# The data set also includes a number of missing observations, indicated by a ?
# using the option header=TRUE in the read.table function tells R 
# that the first line of the file contains the variable names
# using the na.strings tells R that any time it sees a set of characters 
# it should be treated as a missing element of the data matri

Auto <- read.table("Datasets/Auto.data", header=TRUE, na.strings="?", stringsAsFactors=TRUE)
View(Auto)

# The sringsAsFactors=TRUE argument tells R that any variable containing character strings should be interpreted as a qualitative variable,
# and that each distinct character string represents a distinct level for that qualitative variable

# An easy way to load data from Excel to R is to save it as a csv (comma-separated-values) files, and use the read.csv() function
Auto <- read.csv("Datasets/Auto.csv", na.strings="?", stringsAsFactors=T)
View(Auto)

dim(Auto)
# output: The data has 397 rows/observations and 9 columns/variables
Auto[1:4, ]

# there are various ways to deal with missing data.
Auto <- na.omit(Auto)
dim(Auto) # dropped 5 rows to 392 rows and 9 columns

# Once the data are loaded correctly, we can use names() to check the variable names.
names(Auto)

##### Additional Graphical and Numerical Summaries
# We can use the plot() function to produce scatterplots of the quantitative variables.
# However, simply typing the variable names will produce an error message,
# because R does not know to look into the Auto data set for those variables.

plot(cylinders,mpg) # produces an error

plot(Auto$cylinders, Auto$mpg) # use $ symbol to access a column from dataset

# alternatively, we can use the attach() function to tell R
# to make the variables in this data frame available by name.
attach(Auto)
plot(cylinders, mpg)

cylinders # the cylinders variable is stored as a numeric vector

# since there are only a small number of possible values for cylinders,
# one may prefer to treat it as a qualitative variable
# The as.factor() function converts quantitative variables to qualitative variables.

cylinders <- as.factor(cylinders)
plot(cylinders, mpg)
# plotting qualitative variables on the x-axis automatically produces box-plots
# customize the box plots
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders",ylab="MPG")
jpeg("Auto-boxplot.jpeg")
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders",ylab="MPG")
dev.off()

# The hist() function can be used to plot a histogram.
# note that col=2 has same effect as col="red"

hist(mpg)
hist(mpg, col=2)
hist(mpg, col=2, breaks=15)
jpeg("MPG-histogram.jpeg")
hist(mpg, col=2, breaks=15)
dev.off()

# The pairs() function creates a scatterplot matrix
# i.e a scatterplot for every pair of variables.
pairs(Auto)

# We can also produce scatterplots for just a subset of the variables
pairs(
  ~ mpg + displacement + horsepower + weight + acceleration,
  data = Auto
    )
# in conjunction with the plot() function,
# identify() provides a useful interactive method for identifying the value of a particular variable for points on a plot
# We pass in 3 arguments to identify():
# 1. The x-axis variable
# 2. The y-axis variable
# 3. The variable whose values we would like to see printed for each point
plot(horsepower, mpg)
# identify(horsepower,mpg,name) # not responding

# The summary() function produces a numerical summary of each variable in a particular data set.
summary(Auto)

# summary of just a single variable
summary(Auto$mpg)

# Once we have finished using R, we type q() in order to shut it down or quit.
# When exiting R, we have the option to save the current workspace so that all objects (such as data sets) 
# that we created in this R session will be available next time.
# To save a record of all commands that we typed in the session use the savehistory() function.
# Next time we can load that history using the loadhistory() function.