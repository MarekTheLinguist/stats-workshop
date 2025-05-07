### LINEAR REGRESSION ###
## ##
# (you can use hashtags for comments)
## the formula for linear regression:
# y = a + bx
# where y is the value of the outcome variable
# a (called the intercept) is the value of y when x = 0
# b (called the slope) is the rate of change of y over x
# and x is the value of the predictor variable

# a simple scenario - how much the height of a plant
# depends on how much water it gets
height <- c(1, 2, 3, 4, 5, 6, 6, 7, 9, 9, 9, 9, 11)
water <- c(0, 0.3, 0.4, 0.6, 0.7, 0.8, 0.8, 0.9, 1.0, 1.0, 1.0, 1.0, 1.3)
data <- cbind(height, water) #cbind - bind by columns
data <- as.data.frame(data) # change table to a dataframe
data
data$height
data$water
plot(data$height ~ data$water)

# the aim of linear regression is to find a straight line that characterises
# the relationship between x and y
plot(data$height ~ data$water)
abline(h = 6)
# not the best line, right? (by the way, it assumes no relationship)
plot(data$height ~ data$water)
abline(coef = c(1, 5))
# I'm just tryig to eyeball the relationship (first argument of coef is intercept, the second is the slope)
# but it's just guessing - it is not data-driven
# this is where regression can step in :)
## this is called "model fitting": we fit a model to data
simple_model <- lm(data$height ~ data$water) 
# we predict that water affects the height of plants
## the syntax of lm is the following:
# on the left side of ~ you provide y
# on the right side of ~ you provide x
# once we fit a model, we can summarise it to show results
summary(simple_model)
# intercept - what is the predicted height when you do not water the plant 
# (it's negative, so it does not make too much sense)
# we will learn what to do about it later on
# water - our "x" - predictor variable - how it affects height if we add 1 water
# std error - the dispersion of observations from the regression line
# t-value helps in estimating p, so we can ignore it :)
# p value determines the significance of the predictor (is it different from 0)

# we can also fit a regression line to the plot
# you can do it in two ways: using abline and coefficients from the model

plot(data$height ~ data$water)
abline(coef=c(-0.2546, 8.6031))

plot(data$height ~ data$water)
# or by fitting the model itself
abline(simple_model)

# let's check what happens if we put values from the model into the formula
plant_height_1 = -0.2546 + 8.6031 * 1 # what is the predicted height if you add exactly 1 water
#y = a + bx
plant_height_1

plant_height_08 = -0.2546 + 8.6031 * 0.8
plant_height_08
data

# as you can see, there is some error (standard error from the regression line)
# but this is normal! 
# You want a predictive engine, not a line that exactly fits every observation in the data
# modelling is about characterising some general properties of 
# the relationship between x and y
# "All models are wrong, but some are useful" George Box.


### PRACTICE ###
## let's try to predict the relationship between a child's age and their weight
age <- c(5, 5, 6, 6, 6, 7, 7, 8, 8, 9, 9, 10, 11, 12, 13, 14)
weight <- c(14, 17, 19, 18, 19, 21, 21, 23, 24, 25, 25, 31, 29, 32, 33, 34)
## join the two vectors into a table
# your code goes here

data_children <- cbind(age, weight)
data_children <- as.data.frame(data_children)
data_children
## transform the table into a dataframe 
# your code goes here

## plot the data
plot(data_children$weight ~ data_children$age)
# your code goes here

## fit a model to the data, summarise and interpret it
children_model <- lm(weight ~ age, data = data_children)
summary(children_model)
# your code goes here
# your code goes here

## fit a regression line to the plotted data
plot(data_children$weight ~ data_children$age)
abline(children_model)
