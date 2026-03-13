### Load packages
library(tidyverse)
library(bayesplot)
library(janitor)
library(tidybayes)
library(broom.mixed)
library(brms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(posterior)
set.seed(84735)

options(brms.backend = "rstan", mc.cores = 2)


### Loading files and filtering
df <- readr::read_csv("data/filtered_dataset.csv", show_col_types = FALSE)


### Coerce relevant fields to factors
df <- df %>%
  mutate(
    cmpl_constr = as.factor(cmpl_constr),
    lex = as.factor(lex),            
    book_scroll = as.factor(book_scroll),    
    cmpl_anim = as.factor(cmpl_anim),
    cmpl_det = as.factor(cmpl_det),
    cmpl_indiv = as.factor(cmpl_indiv),
    cmpl_complex = as.factor(cmpl_complex),
    motion_type = as.factor(motion_type),
    verse_genre = as.factor(verse_genre),
    era_style = as.factor(era_style)
  )

### Basic sanity: drop rows missing key fields
df <- df %>%
  filter(!is.na(cmpl_constr), !is.na(lex), !is.na(book_scroll))


### Number of book_scrolls and lexemes
nlevels(df$book_scroll)
nlevels(df$lex)

df %>% summarize(
  n_book_scroll   = nlevels(book_scroll),
  n_lex    = nlevels(lex)
)

# Show the book_scrolls categories
table(df$book_scroll)
table(df$cmpl_constr)
table(df$cmpl_anim)
table(df$cmpl_det)
table(df$cmpl_complex)
table(df$motion_type)
table(df$verse_genre)
table(df$era_style, df$book_canonical)

nrow(df[df$lex == "BW>[" & df$cmpl_constr == "dir-he", ])

### Choose baseline categories

levels(df$cmpl_constr)
levels(df$cmpl_det)
levels(df$cmpl_anim)
levels(df$cmpl_anim)

levels(df$era_style)
levels(df$verse_genre)


# Dependent variable
df$cmpl_constr <- relevel(df$cmpl_constr, ref = "prep")

# Individual predictor variables
df$cmpl_anim <- relevel(df$cmpl_anim, ref = "inanim")
df$cmpl_det <- relevel(df$cmpl_det,  ref = "det")
df$cmpl_complex <- relevel(df$cmpl_complex, ref = "simple")
df$motion_type <- relevel(df$motion_type, ref = "factive")

df$verse_genre <- relevel(df$verse_genre, ref = "prose")
df$era_style <- relevel(df$era_style, ref = "CBH")

# Using a df without empty levels
df0 <- droplevels(df)

table(df$cmpl_constr)

### Define priors

# Get priors for book_scroll and lexeme as crossed factors

gp <- get_prior(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type + 
    verse_genre + era_style +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep")
)

gp

# all distributional parameters (one per non-reference category)
dpars <- unique(gp$dpar[gp$class %in% c("b","Intercept") & nzchar(gp$dpar)])

dpars

# Predictors (or fixed effects) priors (I use them for both models, crossed/nested)

# should I define priors for the base levels as well?
# check with 2.5 as priors for dep var
#
gp %>% dplyr::filter(class == "b") %>% dplyr::distinct(coef) %>% dplyr::arrange(coef)

## THE BASIC PRIORS

pri_manual <- c(
  
  # cmpl_animanim (ref = inanim)
  set_prior("normal(0, 1.5)", class="b", coef="cmpl_animanim", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="cmpl_animanim", dpar="muvc"),
  
  # cmpl_complexcomplex (ref = simple)
  set_prior("normal(0, 1.5)", class="b", coef="cmpl_complexcomplex", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="cmpl_complexcomplex", dpar="muvc"),
  
  # cmpl_detund (ref = det)
  set_prior("normal(0, 1.5)", class="b", coef="cmpl_detund", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="cmpl_detund", dpar="muvc"),
  
  # motion_type (ref = factive)
  set_prior("normal(0, 1.5)", class="b", coef="motion_typefictive", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="motion_typefictive", dpar="muvc"),
  
  # verse_genre (ref = prose)
  set_prior("normal(0, 1.5)", class="b", coef="verse_genreinstruction", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="verse_genreinstruction", dpar="muvc"),
  set_prior("normal(0, 1.5)", class="b", coef="verse_genrelist", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="verse_genrelist", dpar="muvc"),
  set_prior("normal(0, 1.5)", class="b", coef="verse_genrepoetry", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="verse_genrepoetry", dpar="muvc"),
  set_prior("normal(0, 1.5)", class="b", coef="verse_genreprophetic", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="verse_genreprophetic", dpar="muvc"),
  
  # era_style (ref = CBH)
  set_prior("normal(0, 1.5)", class="b", coef="era_styleother", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="era_styleother", dpar="muvc"),
  set_prior("normal(0, 1.5)", class="b", coef="era_styleLBH", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="era_styleLBH", dpar="muvc"),
  set_prior("normal(0, 1.5)", class="b", coef="era_styleQH", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="era_styleQH", dpar="muvc"),
  
  # category-specific intercepts for the DV (non-reference categories)
  set_prior("normal(-2, 1)", class="Intercept", dpar="mudirhe"),
  set_prior("normal(-2, 1)", class="Intercept", dpar="muvc")
)

# Group-Level parameters' priors, random-intercept standard deviation (sd) (Crossed model)
pri_re <- do.call(c, lapply(dpars, function(dp) c(
  set_prior("student_t(3, 0, 2.5)", class = "sd", group = "book_scroll", coef = "Intercept", dpar = dp),
  set_prior("student_t(3, 0, 2.5)", class = "sd", group = "lex", coef = "Intercept", dpar = dp)
)))


############################ MODEL 1 - Interaction Motion_type / Era_style ############################

gp <- get_prior(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex +
    motion_type * era_style +
    verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link="logit", refcat="prep")
)

subset(gp, class == "b" & grepl("motion_type.*:era_style|era_style.*:motion_type", coef))

## Specific priors for the interaction

pri_int_motion_era <- c(
  set_prior("normal(0, 1)", class="b", coef="motion_typefictive:era_styleLBH",  dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="motion_typefictive:era_styleLBH",  dpar="muvc"),
  set_prior("normal(0, 1)", class="b", coef="motion_typefictive:era_styleQH",   dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="motion_typefictive:era_styleQH",   dpar="muvc"),
  set_prior("normal(0, 1)", class="b", coef="motion_typefictive:era_styleother",dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="motion_typefictive:era_styleother",dpar="muvc")
)

# add all the priors
priors_interaction_motion_era <- c(pri_manual, pri_re, pri_int_motion_era)

# MODEL

model_interaction_motion_era <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex +
    motion_type * era_style +      # interaction
    verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_interaction_motion_era,
  chains = 4,
  iter = 8000, warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(model_interaction_motion_era, file = "models/models_for_bayes_factor_analysis/m_interaction_motion_era.rds")




############################ MODEL 2 - Interaction cmpl_anim / Era_style ############################

gp <- get_prior(
  cmpl_constr ~  cmpl_det + cmpl_complex +
    cmpl_anim * era_style + 
    motion_type + verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link="logit", refcat="prep")
)

subset(gp, class == "b" & grepl("cmpl_anim.*:era_style|era_style.*:cmpl_anim", coef))

## Specific priors for the interaction

pri_int_anim_era <- c(
  set_prior("normal(0, 1)", class="b", coef="cmpl_animanim:era_styleLBH",  dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_animanim:era_styleLBH",  dpar="muvc"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_animanim:era_styleQH",   dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_animanim:era_styleQH",   dpar="muvc"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_animanim:era_styleother",dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_animanim:era_styleother",dpar="muvc")
)

# add all the priors
priors_interaction_anim_era <- c(pri_manual, pri_re, pri_int_anim_era)

# MODEL

model_interaction_anim_era <- brm(
  cmpl_constr ~ cmpl_det + cmpl_complex +
    cmpl_anim * era_style + # interaction
    motion_type + verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_interaction_anim_era,
  chains = 4,
  iter = 8000, warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(model_interaction_anim_era, file = "models/models_for_bayes_factor_analysis/m_interaction_cmpl_anim_era.rds")


############################ MODEL 3 - Interaction cmpl_det / Era_style ############################

gp <- get_prior(
  cmpl_constr ~ cmpl_anim + cmpl_complex +
    cmpl_det * era_style + 
    motion_type + verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link="logit", refcat="prep")
)

subset(gp, class == "b" & grepl("cmpl_det.*:era_style|era_style.*:cmpl_det", coef))

## Specific priors for the interaction

pri_int_det_era <- c(
  set_prior("normal(0, 1)", class="b", coef="cmpl_detund:era_styleLBH",  dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_detund:era_styleLBH",  dpar="muvc"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_detund:era_styleQH",   dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_detund:era_styleQH",   dpar="muvc"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_detund:era_styleother",dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_detund:era_styleother",dpar="muvc")
)

# add all the priors
priors_interaction_det_era <- c(pri_manual, pri_re, pri_int_det_era)

# MODEL

model_interaction_det_era <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_complex +
    cmpl_det * era_style + # interaction
    motion_type + verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_interaction_det_era,
  chains = 4,
  iter = 8000, warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(model_interaction_det_era, file = "models/models_for_bayes_factor_analysis/m_interaction_cmpl_det_era.rds")


############################ MODEL 4 - Interaction cmpl_complex / Era_style ############################

gp <- get_prior(
  cmpl_constr ~ cmpl_anim + cmpl_det +
    cmpl_complex * era_style + 
    motion_type + verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link="logit", refcat="prep")
)

subset(gp, class == "b" & grepl("cmpl_complex.*:era_style|era_style.*:cmpl_complex", coef))

## Specific priors for the interaction

pri_int_complex_era <- c(
  set_prior("normal(0, 1)", class="b", coef="cmpl_complexcomplex:era_styleLBH",  dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_complexcomplex:era_styleLBH",  dpar="muvc"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_complexcomplex:era_styleQH",   dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_complexcomplex:era_styleQH",   dpar="muvc"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_complexcomplex:era_styleother",dpar="mudirhe"),
  set_prior("normal(0, 1)", class="b", coef="cmpl_complexcomplex:era_styleother",dpar="muvc")
)

# add all the priors
priors_interaction_complex_era <- c(pri_manual, pri_re, pri_int_complex_era)

# MODEL

model_interaction_complex_era <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det +
    cmpl_complex * era_style + # interaction
    motion_type + verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_interaction_complex_era,
  chains = 4,
  iter = 8000, warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(model_interaction_complex_era, file = "models/models_for_bayes_factor_analysis/m_interaction_cmpl_complex_era.rds")