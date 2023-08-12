# 8. Load the College dataset into R using the read.csv() function 
college <- read.csv('Datasets/College.csv')
View(college)
# the first column is the name of each university.
rownames(college) <- college[, 1] # use column 1 as the row index
View(college)

college <- college[, -1] # eliminate the first column from the data
View(college)
# not the first column is Private

# c) Use the summary() function to produce numerical summary of the variables
summary(college)
# the Private column is strings and should be converted to qualitative column
college$Private <- as.factor(college$Private)
summary(college)

# use the pairs() function to produce a scatterplot matrix of the first ten columns of the data
pairs(college[, 1:10])

# use the plot() function to produce side-by-side boxplots of Outstate versus Private
plot(
  college$Private, college$Outstate, 
  col="green", xlab="Private", ylab="Outstate", 
  main="Private Vs Outstate"
  )

# Create a new qualitative variable, called Elite, by binning the Top10Perc variable.
# We divide universities into two groups based on 
# whether or not the proportion of students coming from the top 10% 
# of their high school classes exceeds 50%
### approach 1
Elite <- ifelse(college$Top10perc > 50, 'Yes', 'No')
college$Elite <- as.factor(Elite)
summary(college) # only 78 colleges are elite

### approach 2
# drop the Elite column from first approach
college <- college[, -dim(college)[2]]
names(college) # ensure column was dropped

Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- 'Yes'
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(college) # only 78 colleges are elite

# use the plot() function to produce side-by-side boxplots of Outstate versus Elite
plot(
  college$Elite, college$Outstate,
  col="green", xlab="Elite", ylab="Outstate",
  main="Elite Vs Outstate"
)

### Use the hist() function to produce some histograms 
# with differing number of bins for a few quantitative variables.
par(mfrow=c(3,2))
hist(college$Apps)
#hist(college$Accept)
hist(college$Books)
hist(college$Expend)
hist(college$Outstate)
hist(college$Room.Board, breaks=30)
hist(college$Personal, breaks=20)

# Continue exploring the data, and provide a brief summary of what you discover


#### 9. Auto data set
# make sure that the missing values have been removed from the data.
Auto <- read.csv('Datasets/Auto.csv', stringsAsFactors=TRUE, na.strings="?")
View(Auto)
dim(Auto) # 397 rows 9 columns
# drop the missing values
Auto <- na.omit(Auto)
dim(Auto) # 392 rows 9 columns

# which of the predictors are quantitative, and which are qualitative
summary(Auto)
# qualitative variables: name, origin, cylinders. 
# The year column can also be treated as qualitative with 13 levels

Auto$origin <- as.factor(Auto$origin)
Auto$cylinders <- as.factor(Auto$cylinders)
summary(Auto)

# use the range() function to get the range of each quantitative variable
names(Auto)
range(Auto$mpg) # min: 9 max: 46.6
range(Auto$displacement) # min: 68 max: 455
range(Auto$horsepower) # min: 46 max: 230
range(Auto$weight) # min: 1613 max: 5140
range(Auto$acceleration) # min: 8 max:24.8
range(Auto$year) # min: 70 max:82

# What is the mean and standard deviation of each quantitative variable
mean(Auto$mpg) # Mean of mpg: 23.45
var(Auto$mpg) # Variance of mpg: 60.918
sqrt(var(Auto$mpg)) # Standard Deviation of mpg: 7.805

mean(Auto$displacement) # Mean of displacement: 194.412
var(Auto$displacement) # Variance of displacement: 10950.37
sqrt(var(Auto$displacement)) # Standard Deviation of displacement: 104.644

mean(Auto$horsepower) # Mean of horsepower: 104.469
var(Auto$horsepower) # Variance of horsepower: 1481.57
sqrt(var(Auto$horsepower)) # Standard Deviation of horsepower: 38.49

mean(Auto$weight) # Mean of weight: 2977.584 
var(Auto$weight) # Variance of weight: 721484.7
sqrt(var(Auto$weight)) # Standard Deviation of weight: 849.4026

mean(Auto$acceleration) # Mean of acceleration: 15.5413
var(Auto$acceleration) # Variance of acceleration: 7.611
sqrt(var(Auto$acceleration)) # Standard Deviation of acceleration: 2.76

mean(Auto$year) # Mean of year: 75.98
var(Auto$year) # Variance of year: 13.57
sqrt(var(Auto$year)) # Standard Deviation of year: 3.68

# Now remove the 10th through 85th observations.
# What is the range, mean and standard deviation of each predictor in the subset

summary(Auto[-c(10:85), ])
# range mpg -> min: 11, max:46
# mean mpg: 24.4
sd(Auto[-c(10:85), ]$mpg) # standard deviation of mpg: 7.867

# range displacement -> min: 68, max: 455
# mean displacement: 187.2
sd(Auto[-c(10:85), ]$displacement) # standard deviation of displacement: 99.68

# range horsepower -> min: 46, max: 230
# mean horsepower: 100.7
sd(Auto[-c(10:85), ]$horsepower) # standard deviation of horsepower: 35.71

# range weight -> min: 1649 max: 4997
# mean weight -> 2936
sd(Auto[-c(10:85), ]$weight) # standard deviation of subset weight: 811.3002

# range acceleration -> min: 8.5, max: 24.8
# mean acceleration: 15.73
sd(Auto[-c(10:85), ]$acceleration) # standard deviation of subset accel: 2.69

# range year -> min: 70, max: 82
# mean year: 77.15
sd(Auto[-c(10:85), ]$year) # standard deviation of year: 3.106

#### using the full data set, investigate the predictors graphically,
# using scatterplots or other tools of your choice.
# Create some plots highlighting the relationship among the predictors
pairs(
  ~ mpg + displacement + horsepower + weight + acceleration + year,
  data = Auto
)
par(mfrow=c(1,1))
plot(Auto$displacement, Auto$mpg) # indicates a logarithmic r/ship
plot(Auto$acceleration, Auto$acceleration) # linear r/ship
plot(Auto$horsepower, Auto$mpg) # logarithmic r/ship

# boxplots
plot(
  Auto$cylinders, Auto$mpg,
  xlab="Number of Cylinders", ylab="Miles per gallon")

plot(
  Auto$origin, Auto$mpg,
  xlab="Origin", ylab="Miles per Gallon"
)

plot(
  Auto$cylinders, Auto$horsepower,
  xlab="Number of Cylinders",
  ylab="Horsepower"
)

# histograms 
hist(Auto$mpg)
hist(Auto$horsepower)
hist(Auto$displacement)

# Suppose that we wish to predict gas mileage (mpg) on basis of other variables
# do your plots suggest that any of the other variables might be useful?

# displacement, horsepower and weight exhibit a similar relationship with mpg
# the relationship is logarithmic. 
# one of the variables can be used since they seem correlated
# mpg vs acceleration also seem to be linear related
# year is also shows positively linear r/ship with mpg 

#### The Boston housing data set.

# to begin, load the Boston data set
# install.packages("ISLR2")
# library(ISLR2) # yet to be installed

Boston <- read.csv('Datasets/Boston.csv')
View(Boston)
# drop the index column
Boston <- Boston[, -1]
View(Boston)

dim(Boston) # The Boston dataset has 506 rows/observations, 13 columns/variables

# Do some pairwise plots
pairs(Boston)
summary(Boston)
# chas is categorical variable
Boston$chas <- as.factor(Boston$chas)

# Are any of the predictors associated with per capita crime rate?
plot(Boston$rad, Boston$crim) # crime rate increases with increase in rad distance
plot(Boston$age, Boston$crim) # crime increaes with increase in age

# do any of the census tracts of Boston appear to have
# particularly high crime rates?
# Tax rates?
# pupil -teacher ratios
hist(Boston$crim)
summary(Boston$crim)
# some census tracts have very high crime rates of upto 88.97
which(Boston$crim == max(Boston$crim))
Boston[381,]
max(Boston[-381,]$crim)

summary(Boston$tax)
summary(Boston$ptratio)

# how many of the tracts bound the Charles river?
summary(Boston$chas)
# 35 tracts

# median pupil-teacher ratio
summary(Boston$ptratio)
# median: 19.05

# which tract has lowest edian value of owner-occupied homes?
which(Boston$medv == min(Boston$medv))
Boston[399, ]
Boston[406,]


# how many of the tracts average more than seven rooms per dwelling?
sum(Boston$rm > 7) # 64
# more than 7 rooms per dwelling
sum(Boston$rm > 8) # 13

summary(Boston[Boston$rm>8,])
# has very low crime rate
# on average tax is lower
# houses are older 
# higher proportion of retail business acres per town