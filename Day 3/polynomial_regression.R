library(lme4)
library(afex)
library(ggplot2)
library(tidyverse)


## a very general recap:
# linear regression tries to fit a line that best describes
# the relationship between one outcome and one or more predictor variables
# the formula: y = a + bx
hours_studied <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
test_score <- c(0.5, 1, 2, 3, 4, 5, 6, 7, 9, 10)

plot(test_score ~ hours_studied)
abline(c(0, 0.9))


# but sometimes the relationship is not linear
hours_studied <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
test_score <- test_score ** 2
test_score
plot(test_score ~ hours_studied)
abline(c(0, 0.9))
# fitting a straight line now does not work
# and no straight line will minimise residuals
# in fact, resiudals in here are quite large one way or the other
plot(test_score ~ hours_studied)
abline(c(0, 15))

plot(test_score ~ hours_studied)
abline(c(0, 10))

plot(test_score ~ hours_studied)
abline(c(0, 8))

# to this end, we need to rely on polynomial regression
# here's a small piece of code that does it
data <- cbind(hours_studied, test_score)
data <- as.data.frame(data)
data
# were creating a polynomial orthogonal transformation of hours studied
# degree = says which polynomial degree we want
# we generated the data and know it's quadratic :)
# later on you will see how we decide on the order
# by using orthogonal polynomials instead of natural polynomials
# we avoid collinearity - because as linear term increases, so does a quadratic
# In statistics, collinearity refers to a linear relationship 
# between two explanatory variables. 
# Two variables are perfectly collinear if there is an exact linear relationship between the two,
# so the correlation between them is equal to 1 or −1. 
poly_terms <- poly(data$hours_studied, degree = 2)
poly_terms

poly_terms[,1]

data$time_1 <- poly_terms[, 1]  # First-degree polynomial
data$time_2 <- poly_terms[, 2]  # Second-degree polynomial

data

lin_model <-  lm(test_score ~ time_1, data = data)
poly_model <- lm(test_score ~ time_1 + time_2, data = data)
summary(lin_model)
summary(poly_model)

plot( data$hours_studied, data$test_score, pch = 19, col = "darkgray", main = "2nd Order Polynomial Fit",
     xlab = "x", ylab = "y")

lines(data$hours_studied, fitted(poly_model), col = "darkgreen", lwd = 3)
anova(lin_model, poly_model) # look at RSS: the lower the better
# RSS is residual sum of squares and denotes 
# the discrepancy between the model and the observed data

# it's POLYnomial regression
# y = a + bx
# y = a + b_1 * x + b_2 * x**2
# y = a + b_1 * x + b_2 * x ** 2 + b_3 * x ** 3
# # y = a + b_1 * x + b_2 * x ** 2 + b_3 * x ** 3 + b_4 * x ** 4
# so how do I decide what order polynomial to use?
# you have to visually inspect data
# and look for "inflection points", i.e. moments where the curve changes and how many times
# Set seed for reproducibility
set.seed(123)

# hours studied
hours <- seq(0, 10, by = 0.1)
hours

# Simulate quadratic and cubic relationships with noise
quad_score <- 50 + 3 * hours - 0.5 * hours^2 + rnorm(length(hours), 0, 3)
cubic_score <- 50 + 2 * hours - 0.5 * hours^2 + 0.05 * hours^3 + rnorm(length(hours), 0, 3)

# Fit linear regression models to each
quad_lm <- lm(quad_score ~ hours)
cubic_lm <- lm(cubic_score ~ hours)

# Set up plotting layout
par(mfrow = c(1, 2))  # Two plots side by side

# Quadratic data + linear regression
plot(hours, quad_score, main = "Quadratic Relationship + Linear Fit",
     xlab = "Hours Studied", ylab = "Test Score", pch = 16, col = "gray")
lines(hours, 50 + 3 * hours - 0.5 * hours^2, col = "darkgreen", lwd = 2)  # true curve
abline(quad_lm, col = "red", lwd = 2, lty = 2)  # regression line
legend("bottomright", legend = c("True Quadratic", "Linear Regression"),
       col = c("darkgreen", "red"), lty = c(1, 2), lwd = 2)

# Cubic data + linear regression
plot(hours, cubic_score, main = "Cubic Relationship + Linear Fit",
     xlab = "Hours Studied", ylab = "Test Score", pch = 16, col = "gray")
lines(hours, 50 + 2 * hours - 0.5 * hours^2 + 0.05 * hours^3, col = "blue", lwd = 2)  # true curve
abline(cubic_lm, col = "red", lwd = 2, lty = 2)  # regression line
legend("bottomright", legend = c("True Cubic", "Linear Regression"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Reset plotting layout
par(mfrow = c(1, 1))
dev.off()
## let's use some data from a coursebook
# the coursebook is entitled "Growth Curve Analysis and Visualization with R"
# by Daniel Mirman
# download rdata https://dmirman.github.io/GCA/Examples.Rdata
all_files <- load("~/Downloads/Examples.Rdata") # provide path to the file on your computer
all_files # contents of rdata
# now you can load any datafile from all_files

WordLearnEx
head(WordLearnEx)
view(WordLearnEx)

# Subject: A unique identifier for each participant. 
# TP: A categorical between-participants factor with two levels, low and high (within-participants manipulations will be covered in Chapter 4).
# Block: A numeric variable indicating training block, ranging from 1 to 10.
# Accuracy: Proportion correct for a given participant in a given training block, ranging from 0 to 1.

WordLearnEx$TP <- as_factor(WordLearnEx$TP)

ggplot(WordLearnEx, aes(x = Block, y = Accuracy, shape = TP)) +
  stat_summary(fun = mean, geom="line", linewidth=1) +
  stat_summary(fun.data=mean_se, geom="pointrange", linewidth=1) +
  theme_bw(base_size=10) +
  coord_cartesian(ylim=c(0.5, 1.0)) +
  scale_x_continuous(breaks=1:10)


ggplot(WordLearnEx, aes(Block, Accuracy, color = TP)) +
  stat_summary(fun = mean, geom="line", size=1) +
  stat_summary(fun.data=mean_se, geom="pointrange", linewidth=1) +
  theme_bw(base_size=10) +
  coord_cartesian(ylim=c(0.5, 1.0)) +
  scale_x_continuous(breaks=1:10)

# create a second-order polynomial
polynomial <- poly(unique(WordLearnEx$Block), 2)
polynomial
WordLearnEx[,paste("ot", 1:2, sep="")] <- polynomial[WordLearnEx$Block, 1:2]
WordLearnEx

linear_model <- lm(Accuracy ~ Block * TP, data = WordLearnEx)
summary(linear_model)  
# INTERPRETATION
# accuracy increases with block, high TP does not increase accuracy
# and neither does interaction between block and tp high (no additional "boost" from TP)
ggplot(WordLearnEx, aes(Block, Accuracy, shape=TP)) +
  stat_summary(aes(y=fitted(linear_model), linetype=TP), fun.y=mean,
               geom="line", size=1) +
  stat_summary(fun.data=mean_se,geom="pointrange",size=1)+
  theme_bw(base_size=10) +
  coord_cartesian(ylim=c(0.5, 1.0)) +
  scale_x_continuous(breaks=1:10)

quadratic_model <- lm(Accuracy ~ (ot1+ot2) * TP, data=WordLearnEx)
summary(quadratic_model)

# INTERPRETATION

#ot1 (0.286315):
#This is the linear effect of your continuous variable (say, time) in the baseline group (when TPHigh = 0). A positive and significant coefficient means the outcome increases over time in a linear fashion for the baseline group.

# ot2 (-0.050849):
# This is the quadratic effect (curvature) of time in the baseline group. 
# It’s negative but not significant (p = 0.167), suggesting some downward bend, 
# but weak evidence for non-linearity.

# TPHigh (0.052961):
# This is the effect of being in the TPHigh group when time = 0. It’s significant (p = 0.00136), so at the starting point, TPHigh has a higher outcome than the baseline group.

# ot1:TPHigh (0.001075):
# This is the interaction between time (linear) and TPHigh. 
# It's not significant at all (p = 0.98), suggesting the rate of change over time is not different between TPHigh and the baseline group.

# ot2:TPHigh (-0.116455):
# This interaction is significant (p = 0.0256), 
# and it implies that the curvature over time is more negative for the TPHigh group compared to the baseline.
 

ggplot(WordLearnEx, aes(Block, Accuracy, shape=TP)) +
  stat_summary(aes(y=fitted(quadratic_model), linetype=TP), fun.y=mean,
               geom="line", size=1) +
  stat_summary(fun.data=mean_se,geom="pointrange",size=1)+
  theme_bw(base_size=10) +
  coord_cartesian(ylim=c(0.5, 1.0)) +
  scale_x_continuous(breaks=1:10)


anova(linear_model, quadratic_model)

# now your turn!

all_files

## use this data
view(CP)

ggplot(CP, aes(x = Stimulus, y = d.prime, color = Type)) +
  stat_summary(fun = mean, geom="line", linewidth=1) +
  stat_summary(fun.data=mean_se, geom="pointrange", linewidth=1) +
  theme_bw(base_size=10)
# The CP data frame contains auditory discrimination data (d', called “d prime”)
# for two continua of eight stimuli. The continua were created by morphing between 
# two sounds from different categories, either along a temporal acoustic
# dimension or along a spectral acoustic dimension. The hypothesis was that
# there would be “categorical perception” — better discrimination near the category 
# boundary than near the endpoints — for the temporal dimension but not for the spectral dimension

# visualise the data using ggplot (d.prime ~ stimulus * type)
# based on the observation, decide which order polynomial will work best
# create an n-order polynomial and add it to your data
# model it
# visualise the data and fit a curve to the model
# compare a linear model with a quadratic model


plot(CP$Stimulus, CP$d.prime, xlab = "Block", ylab = "d.prime", pch = 16, col = CP$Type)

CP$Type <- as_factor(CP$Type)

ggplot(CP, aes(x = Stimulus, y = d.prime, color=Type)) +
  stat_summary(fun = mean, geom="line", size=1) +
  stat_summary(fun.data=mean_se, geom="pointrange", linewidth=1)



polynomial <- poly(unique(CP$Stimulus), 2) 
CP[,paste("ot", 1:2, sep="")] <- polynomial[CP$Stimulus, 1:2]

mdl_lin <- lm(d.prime ~ Stimulus * Type, data = CP)
summary(mdl_lin)

mdl_quad <- lm(d.prime ~ (ot1+ot2) * Type, data = CP)
summary(mdl_quad)

# negative trend for the temporal discrimination: people are better "in the middle"
# spectral dimension - better at the endpoints?

ggplot(CP, aes(Stimulus, d.prime, shape=Type)) +
  stat_summary(aes(y=fitted(mdl_quad), linetype=Type), fun.y=mean,
               geom="line", size=1) +
  stat_summary(fun.data=mean_se,geom="pointrange",size=1)
