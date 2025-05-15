library(lme4)
library(afex)
library(tidyverse)
library(ggplot2)

# logistic regression is the application of linear regression to solving 
# problems when the outcome is binary (it takes values such as 0 or 1:
# male vs female, noun vs verb, success vs failure etc.)
# healthy vs ill 

hours_slept <- c(1, 1, 2, 5, 6, 6, 1, 2, 4, 5, 7.5, 7.5, 7.5, 8, 9, 9, 1, 1, 2, 5, 6, 6, 1, 2, 4, 5, 7.5, 7.5, 7.5, 8, 9, 9)
came_to_the_workshop <- c(0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 0)
# bind
simple_data <- cbind(hours_slept, came_to_the_workshop)
# transform
simple_data <- as.data.frame(simple_data)
# change variable type (by default it's numeric)
simple_data$came_to_the_workshop <- as_factor(simple_data$came_to_the_workshop)
simple_data$came_to_the_workshop

simple_log_m <- glm(came_to_the_workshop ~ hours_slept, family = "binomial", data = simple_data)
summary(simple_log_m)
levels(simple_data$came_to_the_workshop)
# so what can we say?
# there is a positive relationship, but how and of what, what is the unit?
# it's log odds - logits
# log odds = log(p/(1-p)) # where log is the natural logarithm (base e - check exp(1) in console) 
# and p is the probability of an event
# odds (p / (1-p)) are the probability of an event occurring (p) 
# divided over the probability of an event not occuring (1-p)
# how can we extract probabilities from odds?
# plogis function

# let's calculate odds for someone who slept 1 hour and 9 hours
intercept <- -0.9630
slope <- 0.3056

party_goer <- intercept + slope * 1 #(slept one hour)
party_goer
goody_2_shoes <- intercept + slope * 9 # (slept 9 hours)
goody_2_shoes

plogis(party_goer)
plogis(goody_2_shoes)

# we can also do that for the whole dataframe
simple_data$logodds <- intercept + slope * simple_data$hours_slept

simple_data$probability <- plogis(simple_data$logodds) 
simple_data


ggplot(simple_data, aes(x=hours_slept, y=probability)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se = FALSE) 

# let's try repeating the steps with "real" data
data <- read_csv("~/Documents/Research/stats-workshop/Day 2/annotations_psych.csv")
data
view(data)
data$round <- str_sub(data$round, -1)
data$round
data$resp
data$resp <- as_factor(data$resp) # this is the response from the praticipants - "left" wrong, "right" correct
data$resp
data$round <- as.numeric(data$round)
data$round
data$condition

data <- data %>% mutate_at(c("condition", "director", "resp", "pair"), as_factor)

logreg <- glm(resp ~ round, family = "binomial", data = data)
summary(logreg)

intercept <- -0.67084
slope <- 1.31354

data$logodds <- intercept + slope * data$round
data$prob <- plogis(data$logodds)


data$prob

ggplot(data, aes(x = round, y = prob)) +
  geom_point(shape = "|", size = 6, na.rm = TRUE, aes(color = factor(round))) +
  geom_smooth(method = glm, method.args = list(family = binomial), na.rm = TRUE, 
              formula = y ~ x, color = "navy", fill = "lightblue") +
  coord_cartesian(ylim = c(0, 1), expand = 0) +
  labs(x = "Round", y = "Correct answer probability") +
  theme_minimal()

## 
mdl_inter <- glm(resp ~ round * condition, family = "binomial", data = data)
levels(data$condition)
summary(mdl_inter)
library(car)
data_cars <- mtcars
data_cars$vs
## model vs (vertical vs straight engine) as an outcome of wt
data_cars$vs <- as_factor(data_cars$vs)
data_cars$vs

mdl_vs <- glm(vs ~ wt, family = "binomial", data = data_cars)
summary(mdl_vs)
# calculate plogis for the whole table
intercept <- 5.7147
slope <- -1.9105

data_cars$logodds <- intercept + slope * data_cars$wt
data_cars$probability <- plogis(data_cars$logodds)
data_cars$probability
# plot probabilities
ggplot(data_cars, aes(x=wt, y=probability)) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"), se = FALSE)

