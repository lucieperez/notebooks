library(brms)
library(bayestestR)


# MODELS
full_model <- readRDS("models/models_for_bayes_factor_analysis/m_full.rds")
empty_model <- readRDS("models/models_for_bayes_factor_analysis/m_empty.rds")
complexity_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_complexity.rds")
animacy_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_animacy.rds")
defin_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_definiteness.rds")
era_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_era.rds")
genre_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_genre.rds")
motion_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_motion_type.rds")
individuation_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_individuation.rds")

# 5 predictor models

m5_anim_removed <- readRDS("models/models_for_bayes_factor_analysis/m5_anim_removed.rds")
m5_det_removed <- readRDS("models/models_for_bayes_factor_analysis/m5_det_removed.rds")
m5_complex_removed <- readRDS("models/models_for_bayes_factor_analysis/m5_complex_removed.rds")
m5_indiv_removed <- readRDS("models/models_for_bayes_factor_analysis/m5_indiv_removed.rds")
m5_verse_genre_removed <- readRDS("models/models_for_bayes_factor_analysis/m5_verse_genre_removed.rds")
m5_era_removed <- readRDS("models/models_for_bayes_factor_analysis/m5_era_removed.rds")


# Comparison




##################### COMPARISONS WITHOUT EMPTY MODEL ##################### 

comparison_1_empty <- bayesfactor_models(full_model, 
                                 complexity_removed,
                                 animacy_removed,
                                 defin_removed,
                                 era_removed,
                                 genre_removed,
                                 motion_removed,
                                 individuation_removed,
                                 denominator = empty_model)

comparison_1_empty

##################### COMPARISONS WITH FULL MODEL ##################### 

comparison_2_full <- bayesfactor(
  complexity_removed,
  animacy_removed,
  defin_removed,
  era_removed,
  genre_removed,
  motion_removed,
  individuation_removed,
  denominator = full_model
)

comparison_2_full

##################### OTHER COMPARISONS ##################### 

# Comparison of motion_removed (6 predictors) with the 5 predictors models (motion absent from all models)

comparison_3_motion_removed <- bayesfactor(
  m5_anim_removed,
  m5_det_removed,
  m5_complex_removed,
  m5_indiv_removed,
  m5_verse_genre_removed,
  m5_era_removed,
  denominator = motion_removed
)

comparison_3_motion_removed

# Save results to csv files

# ============================================================
# comparison_1_empty
# ============================================================

comparison_1_empty_df <- as.data.frame(comparison_1_empty)

comparison_1_empty_df$model_id <- rownames(comparison_1_empty_df)

comparison_1_empty_df$BF <- exp(comparison_1_empty_df$log_BF)

comparison_1_empty_df <- comparison_1_empty_df[, c("model_id", "Model", "log_BF", "BF")]

rownames(comparison_1_empty_df) <- NULL

write.csv(
  comparison_1_empty_df,
  "statistical_tests_results/bayes_factor_analyses/comparison_1_empty.csv",
  row.names = FALSE
)


# ============================================================
# comparison_2_full
# ============================================================

comparison_2_full_df <- as.data.frame(comparison_2_full)

comparison_2_full_df$model_id <- rownames(comparison_2_full_df)

comparison_2_full_df$BF <- exp(comparison_2_full_df$log_BF)

comparison_2_full_df <- comparison_2_full_df[, c("model_id", "Model", "log_BF", "BF")]

rownames(comparison_2_full_df) <- NULL

write.csv(
  comparison_2_full_df,
  "statistical_tests_results/bayes_factor_analyses/comparison_2_full.csv",
  row.names = FALSE
)


# ============================================================
# comparison_3_motion_removed
# ============================================================

comparison_3_motion_removed_df <- as.data.frame(comparison_3_motion_removed)

comparison_3_motion_removed_df$model_id <- rownames(comparison_3_motion_removed_df)

comparison_3_motion_removed_df$BF <- exp(comparison_3_motion_removed_df$log_BF)

comparison_3_motion_removed_df <- comparison_3_motion_removed_df[, c("model_id", "Model", "log_BF", "BF")]

rownames(comparison_3_motion_removed_df) <- NULL

write.csv(
  comparison_3_motion_removed_df,
  "statistical_tests_results/bayes_factor_analyses/comparison_3_motion_removed.csv",
  row.names = FALSE
)
