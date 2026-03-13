library(brms)
library(bayesplot)

# MODELS
motion_verb_1 <- readRDS("models/motion_verb_1.rds")

# Prior summary
prior_summary(motion_verb_1)

# Summary
summary(motion_verb_1)

#population level variables
fixef(motion_verb_1)

#obtain the odds ratio (exponential of the log-odds)

exp(fixef(motion_verb_1))

# ranef, to see a list with the group-level effects
# of each grouping variables

# BOOK SCROLL
# TODO: add with the row names
# TODO: make decent plots, turn 90 degrees, errors bars, verbs/book names

random_effects <- ranef(motion_verb_1)
dir_he_ranef <- random_effects$book_scroll[, , "mudirhe_Intercept"]
str(dir_he_ranef)
dir_he_ranef <- as.data.frame(dir_he_ranef)

dir_he_estimates <- dir_he_ranef$Estimate
sort(dir_he_estimates)
plot(sort(dir_he_estimates))


# LEXEMES

# extract Estimate column

# DIRECTIVE HE
dir_he_ranef_lex <- random_effects$lex[, , "mudirhe_Intercept"]
str(dir_he_ranef_lex)
dir_he_ranef_lex <- as.data.frame(dir_he_ranef_lex)

dir_he_estimates_lex <- dir_he_ranef_lex$Estimate
sort(dir_he_estimates_lex)
plot(sort(dir_he_estimates_lex))


# plot the conditional effects
# this computes predicted probabilities for each outcome category

conditional_effects(categorical=TRUE, motion_verb_1)
