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
data_et <- read_csv("~/Documents/Research/stats-workshop/Day 2/data.csv")
view(data_et)
# transform "RECORDING_SESSION_LABEL", "Condition", "IA_LABEL", "Session_Name_", "source_target", "map_sentence"
# create a new variable, called data_map, which filters data_et on the basis of the column source_target and selects only target
# build a simple linear model which checks if mean fixation duration is affected by condition and trial index

# now, let's build a model with random effects with a different subset of the data
data_map <- filter(data_et, data$map_sentence == "map")


mdl_simp_map <- lm(mean_fixation_duration ~ Condition, data = data_map)

summary(mdl_simp_map)

mdl_both_mfd_simp <- lm(mean_fixation_duration ~ Condition * source_target * Trial_Index_, data = data_map)
summary(mdl_both_mfd_simp)

# model with random effects for participant

mdl_condition <- lmer(mean_fixation_duration ~ Condition + (1 |Session_Name_), data = data_map)
mdl_condition_st <- lmer(mean_fixation_duration ~ Condition * source_target + (1 |Session_Name_), data = data_map)
mdl_full_mfd <- lmer(mean_fixation_duration ~ Condition * source_target * Trial_Index_+ (1 |Session_Name_), data = data_map)
summary(mdl_full_mfd)




mdl_null <- lmer(mean_fixation_duration ~ 1 + (1 |Session_Name_), data = data_map)

# do model comparisons
anova(mdl_null, mdl_condition, mdl_condition_st, mdl_full_mfd)


# we do the same with random effects structure
data <- read_csv("~/Documents/Research/stats-workshop/Day 1/data_4.csv")
data <- data %>% mutate_at(c("meaning", "participant", "pair", "condition", "round"), as_factor)

data_path <- filter(data, data$totalPath < 900)
ggplot(data_path, aes(x = round, y = totalPath, fill = condition)) +
  geom_boxplot()


mdl_null <- lmer(totalPath ~ 1 + (1|participant), data = data_path)

mdl_condition <- lmer(totalPath ~ condition + (1|participant), data = data_path)
mdl_condition_round <- lmer(totalPath ~ condition * round + (1|participant), data = data_path)

mdl_condition_round_pair <-  lmer(totalPath ~ condition * round + (1|participant) + (1|pair),  data = data_path)
mdl_condition_round_pair_meaning <-  lmer(totalPath ~ condition * round + (1|participant) + (1|pair) + (1|meaning),  data = data_path)
mdl_condition_round_pair_meaning_2 <-  lmer(totalPath ~ condition * round + (1|participant:pair) + (1|pair) + (1|meaning),  data = data_path)

anova(mdl_null, mdl_condition_round, mdl_condition_round_pair, mdl_condition_round_pair_meaning, mdl_condition_round_pair_meaning_2)
# rule of parsimony

