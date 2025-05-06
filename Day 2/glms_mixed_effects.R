library(lme4)
library(afex)
library(tidyverse)
library(ggplot2)

# so far we've been silently assuming that all observations were independent
# but this is not true in many cases: in the data from the experiment, the participant
# answered a couple of times - it can't really be said that the observation of the answer
# is independent. The participant could have responded to the stimulus basing on their prior answer.
# score on a student's test can be a function of how much they studied, 
# but also the teacher could have affected their performance.
# One answer to this problem is averaging: you average each student's scores and so you have one 
# observation per student. But in this sense you are losing some resolution of the data.
# The best solution for that is using RANDOM EFFECTS.
# (now we need to distinguish between fixed effects - the ones that we
# are interested in investigating, e.g. response times, and random effects -
# ones that can potentially affect the model but are not part of our design)

set.seed(42)
n_schools <- 10
students_per_school <- 30
school_ids <- factor(rep(1:n_schools, each = students_per_school))

# Random intercepts for schools
school_effects <- rnorm(n_schools, mean = 0, sd = 10)
intercepts <- school_effects[as.numeric(school_ids)]

# Predictor: study hours
study_hours <- rnorm(n_schools * students_per_school, mean = 5, sd = 2)

# Outcome: test scores
test_scores <- 50 + 5 * study_hours + intercepts + rnorm(n_schools * students_per_school, sd = 5)

# Create data frame
data <- data.frame(school = school_ids, study_hours = study_hours, test_scores = test_scores)
data
# Fit regular linear model (ignores school clustering)
lm_model <- lm(test_scores ~ study_hours, data = data)

# Fit random intercept model
lmer_model <- lmer(test_scores ~ study_hours + (1 | school), data = data)

# Predict values for plotting
data$lm_pred <- predict(lm_model)
data$lmer_pred <- predict(lmer_model)

# Plotting
ggplot(data, aes(x = study_hours, y = test_scores, color = school)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = lm_pred), color = "black", size = 3, linetype = "dashed") +
  geom_line(aes(y = lmer_pred), size = 1) +
  labs(title = "Linear Model vs Random Intercept Model",
       subtitle = "Dashed black = Linear Model; Colored lines = Random Intercept Model",
       x = "Study Hours", y = "Test Scores") +
  theme_minimal()


" This is the average effect of study hours across all schools — it’s the slope you’d get in a standard linear regression:

    “On average, each additional hour of study increases test scores by 5 points.”

This is a single, global estimate — it's the population-level effect.
2. Random Slopes:

Random slopes allow the effect of study hours (i.e., the slope) to vary across groups (in this case, schools). So, each school gets its own slope:

    “In School A, one hour of study increases scores by 6 points; in School B, it only increases by 3.”

These slopes are drawn from a distribution centered around the fixed effect slope."


# now we can try working with a real-life example



# do model comparisons
# maximal model
# rule of parsimony

