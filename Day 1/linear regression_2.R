## R has many functionalities built-in, but there are also
## additional packages that we can use to extend its capabilities
## I recommend using the following:
library(tidyverse) # for loading data from disk and manipulating it
library(ggplot2) # for data visualisation
library(lme4) # for mixed-effects models
library(afex) # for significance testing in mixed effects models

# we will now use actual data collected from an experiment
# we need to load it first 
# you have to locate it on your drive (Windows - right click, show path)
data <- read_csv("~/Documents/Research/stats-workshop/Day 1/data_4.csv")
## the data file has the following variables:
# meaning - what the participants communicated about
# participant - participant id
# pair - pair id
# condition - one of two conditions, sitting and standing
# round - round of the "game"
# totalPath - distance travelled by participants (cm)
# totalAmplitude - distance (in a straight line) between two most extreme points
# totalBbmv - area of movement (cm^3)
# totalVelocity - how fast the movement was
# and some others :)
# it's generally a good idea to eye the data before you do any modelling
mean(data$totalConvexHull)
median(data$totalConvexHull)
plot(data$totalConvexHull ~ data$round)

data <- filter(data, data$totalConvexHull < 1000)
plot(data$totalConvexHull ~ data$round)

# there are some major outliers, which we may want to filter out for now
# (you will learn what to do so as not to filter out data later)

# and model it
mdl1 <- lm(totalConvexHull ~ round, data = data)
summary(mdl1)
# the model predicts a negative influence of round on amplitude:
# the "bigger" the round, the lower the convex hull
# (which is good, considering my hypothesis)

# we can plot it somewhat better using ggplot

ggplot(data, aes(x = as_factor(round), y = totalConvexHull, fill = as_factor(round))) +
  geom_boxplot() +
  theme_minimal()

# your turn! repeat the steps for total bbmv

## as we have seen in the ggplot for amplitude, turn can be 
## seen as a linear predictor or a categorical predictor
## since there are three rounds of the game (and no values in between,
## like 1.5, 1.75), and the variable can only be one of the three values
## perhaps it's a better idea to treat it as a categorical predictor
## we need to transform it first:

data$round <- as_factor(data$round)
## check the variable now
levels(data$round) # the variable has three ordered levels, "1", "2", "3"
## R orders variables in an ascending order (for numerals) or alphabetically (for strings)
## let's see how the interpretation of the model changes when we have changed the variable type
data_amp <- filter(data, totalAmplitude < 500)

mdl2 <- lm(totalAmplitude ~ round, data = data_amp)
summary(mdl2)
## now, we have an intercept (as always)
## and two coefficients
## the estimate is the predicted difference between the value of the coefficient and the intercept (!!!!)
## so we have -5.42 for the difference between round 2 and round 1
## and -5.44 for the difference between round 3 and round 1


## if you are interested in the difference between subsequent levels, 
## you have to recode the data:
# https://stats.oarc.ucla.edu/r/library/r-library-contrast-coding-systems-for-categorical-variables/
## this can be achieved by backward difference coding
# luckily, there's a library for that :)
library(MASS)
install.packages("MASS")
mdl3 <- lm(totalAmplitude ~ round, data = data_amp, contrasts = list(round = MASS::contr.sdif))
summary(mdl3)
## the intercept stays more or less the same
## but check what happened to predictors
## the estimated value for round2-1 is the same as in the previous model 
## (because it's a comparison against the intercept, just like in the previous model)
## but round3-2 is very small and statistically insignificant 

# now do the same with bbmv and interpret the results


## INTERACTIONS 
# sometimes the change in the outcome variable
# can be attributed to more than one predictor
# like plant growth - it's not only water, it's also the sun

# here, we have two conditions and three rounds - perhaps the rate of change
# is different in there; we specify an interaction term by *
# (if you want to investigate them separately, you use a + instead of *)
data_amp$condition <- as_factor(data_amp$condition)

mdl_inter1 <- lm(totalAmplitude ~ round * condition, data = data_amp)
summary(mdl_inter1)
## here's how you can plot it

ggplot(data_amp, aes(x = round, y = totalAmplitude, fill = condition)) +
  geom_boxplot()
# or
ggplot(data_amp, aes(x = condition, y = totalAmplitude, fill = round)) +
  geom_boxplot()
# it depends on what you want to foreground


## you can have also interactions between continuous variables
library(car)
data_cars <- mtcars
# data description can be found here https://rstudio-pubs-static.s3.amazonaws.com/61800_faea93548c6b49cc91cd0c5ef5059894.html
data_cars

# say we want to predict mpg on some of the variables in the dataframe
# e.g. the number of cylinders and displacement

car_mpg <- lm(mpg ~ cyl * disp, data = data_cars)
summary(car_mpg)
# the prolem with the results is that the number of cylinders and displacement are on completely differen scales
range(data_cars$cyl)
range(data_cars$disp)
# hence, a one unit increase in the number of cylinders is a huge difference
# and in displacement it's barely any difference
# the solution to this problem is to put both observations on the same scale
# one such scale is z-scoring (standardisation): subtracting an observation from the mean value of observations
# and then dividing the difference by the standard deviation of observations
# (this is the first way to transform your data, the other is logarithmic transformation)
data_cars$cyl_z <- (data_cars$cyl - mean(data_cars$cyl)) / sd(data_cars$cyl)
data_cars$disp_z <- (data_cars$disp - mean(data_cars$disp)) / sd(data_cars$disp)

data_cars

car_mpg_new <- lm(mpg ~ cyl_z * disp_z, data = data_cars)
summary(car_mpg_new)

## and now we have a different story! 
# try the same with drat and wt and mpg as outcome


# now, to log transformation
# log transformation is the opposite of exponentiation
log(8, 2) # the first argument is the result of the logarithm, the second is the base
# this means that you need to raise 2 to the power of 3 to obtain 8
# by default, R uses natural logarithms 
log(8)
exp(1) ** 2.079442 # almost there ;) 
# exp(1) gives you euler's number
exp(1)
# let's get back to our convex hull example
data <- read_csv("~/Documents/Research/stats-workshop/Day 1/data_4.csv")
data$c_hull_log <- log(data$totalConvexHull)


ggplot(data, aes(x = as.factor(round), y = data$c_hull_log)) +
  geom_boxplot()

mdl_log <- lm(c_hull_log ~ as.factor(round), data = data)
summary(mdl_log) # no data wasted, nice results obtained
# log trasnformation is very useful when dealing with positive skeweness
# that is, when there is a lot of data going towards positive infinity

