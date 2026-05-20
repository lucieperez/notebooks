# Packages 

library(brms)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(posterior)
library(performance)
library(parameters)
library(see)


# Import the model

best_model <- readRDS("models/models_for_bayes_factor_analysis/m_without_motion_type.rds")

# Model overview


best_model
summary(best_model)
formula(best_model)
prior_summary(best_model)