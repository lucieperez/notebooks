library(brms)
library(bayestestR)


# MODELS
full_model <- readRDS("models/motion_verb_1.rds")
empty_model <- readRDS("models/models_for_bayes_factor_analysis/m_empty.rds")
complexity_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_complexity.rds")
animacy_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_animacy.rds")
defin_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_definiteness.rds")
era_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_era.rds")
genre_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_genre.rds")
motion_removed <- readRDS("models/models_for_bayes_factor_analysis/m_without_motion_type.rds")

m_int_motion_era <- readRDS("models/models_for_bayes_factor_analysis/m_interaction_motion_era.rds")
m_int_anim_era <- readRDS("models/models_for_bayes_factor_analysis/m_interaction_cmpl_anim_era.rds")
m_int_complex_era <- readRDS("models/models_for_bayes_factor_analysis/m_interaction_cmpl_complex_era.rds")
m_int_det_era <- readRDS("models/models_for_bayes_factor_analysis/m_interaction_cmpl_det_era.rds")

# Comparison

comparison_1_empty <- bayesfactor_models(full_model, 
                                 complexity_removed,
                                 animacy_removed,
                                 defin_removed,
                                 era_removed,
                                 genre_removed,
                                 motion_removed,
                                 denominator = empty_model)

comparison_1_empty



comparison_2_empty <- bayesfactor_models(full_model, 
                                 m_int_anim_era,
                                 m_int_det_era,
                                 m_int_motion_era,
                                 m_int_complex_era,
                                 denominator = empty_model)

comparison_2_empty



##################### COMPARISONS WITHOUT EMPTY MODEL ##################### 

comparison_1_full <- bayesfactor_models( 
                                 complexity_removed,
                                 animacy_removed,
                                 defin_removed,
                                 era_removed,
                                 genre_removed,
                                 motion_removed,
                                 denominator = full_model)

comparison_1_full



comparison_2_full <- bayesfactor_models( 
                                   m_int_anim_era,
                                   m_int_det_era,
                                   m_int_motion_era,
                                   m_int_complex_era,
                                   denominator = full_model)

comparison_2_full
