# Packages
library(loo)

# Import models
full_model <- readRDS("models/models_for_bayes_factor_analysis/m_full.rds")
empty_model <- readRDS("models/models_for_bayes_factor_analysis/m_empty.rds")
complexity_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_complexity.rds")
animacy_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_animacy.rds")
defin_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_definiteness.rds")
era_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_era.rds")
genre_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_genre.rds")
motion_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_motion_type.rds")
individuation_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_individuation.rds")

# Add all loo to list
loo_list <- list(
  full = loo(full_model),
  without_complexity = loo(complexity_removed),
  without_animacy = loo(animacy_removed),
  without_definiteness = loo(defin_removed),
  without_era = loo(era_removed),
  without_genre = loo(genre_removed),
  without_motion = loo(motion_removed),
  without_individuation = loo(individuation_removed)
)

# Conduct LOO comparsion
loo_compare(loo_list)