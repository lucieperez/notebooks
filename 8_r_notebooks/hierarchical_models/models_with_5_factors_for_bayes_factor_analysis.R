# SECTION 1: Packages, Data and group-level priors

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
table(df$cmpl_indiv)
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
levels(df$cmpl_indiv)
levels(df$era_style)
levels(df$verse_genre)


# Dependent variable
df$cmpl_constr <- relevel(df$cmpl_constr, ref = "prep")

# Individual predictor variables
df$cmpl_anim <- relevel(df$cmpl_anim, ref = "inanim")
df$cmpl_det <- relevel(df$cmpl_det,  ref = "det")
df$cmpl_complex <- relevel(df$cmpl_complex, ref = "simple")
df$cmpl_indiv <- relevel(df$cmpl_indiv, ref = "nmpr")
df$motion_type <- relevel(df$motion_type, ref = "factive")

df$verse_genre <- relevel(df$verse_genre, ref = "prose")
df$era_style <- relevel(df$era_style, ref = "CBH")

# Using a df without empty levels
df0 <- droplevels(df)

table(df$cmpl_constr)

### Define priors

# Get priors for book_scroll and lexeme as crossed factors

gp <- get_prior(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + cmpl_indiv + motion_type + 
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



# Group-Level parameters' priors, random-intercept standard deviation (sd) (Crossed model)
pri_re <- do.call(c, lapply(dpars, function(dp) c(
  set_prior("student_t(3, 0, 2.5)", class = "sd", group = "book_scroll", coef = "Intercept", dpar = dp),
  set_prior("student_t(3, 0, 2.5)", class = "sd", group = "lex", coef = "Intercept", dpar = dp)
)))

# Define variables with priors for each predictors, easy to use when defining model specific priors later on

## Animacy: cmpl_anim (ref = inanim)
pri_cmpl_anim <- c(
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_animanim", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_animanim", dpar = "muvc")
)

## Complexity: cmpl_complex (ref = simple)
pri_cmpl_complex <- c(
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_complexcomplex", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_complexcomplex", dpar = "muvc")
)

## Definiteness: cmpl_det (ref = det)
pri_cmpl_det <- c(
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_detund", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_detund", dpar = "muvc")
)

## Individuation: cmpl_indiv
pri_cmpl_indiv <- c(
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivadj", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivadv", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivsubs", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivppde", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivppin", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivprsf", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivptcl", dpar = "mudirhe"),
  
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivadj", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivadv", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivsubs", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivppde", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivppin", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivprsf", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "cmpl_indivptcl", dpar = "muvc")
)

## Era/style: era_style (ref = CBH)
pri_era_style <- c(
  set_prior("normal(0, 1.5)", class = "b", coef = "era_styleLBH", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "era_styleother", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "era_styleQH", dpar = "mudirhe"),
  
  set_prior("normal(0, 1.5)", class = "b", coef = "era_styleLBH", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "era_styleother", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "era_styleQH", dpar = "muvc")
)

## Motion type: motion_type (ref = factive)
pri_motion_type <- c(
  set_prior("normal(0, 1.5)", class = "b", coef = "motion_typefictive", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "motion_typefictive", dpar = "muvc")
)

## Verse genre: verse_genre (ref = prose)
pri_verse_genre <- c(
  set_prior("normal(0, 1.5)", class = "b", coef = "verse_genreinstruction", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "verse_genrelist", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "verse_genrepoetry", dpar = "mudirhe"),
  set_prior("normal(0, 1.5)", class = "b", coef = "verse_genreprophetic", dpar = "mudirhe"),
  
  set_prior("normal(0, 1.5)", class = "b", coef = "verse_genreinstruction", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "verse_genrelist", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "verse_genrepoetry", dpar = "muvc"),
  set_prior("normal(0, 1.5)", class = "b", coef = "verse_genreprophetic", dpar = "muvc")
)

## Category-specific intercepts for the DV
pri_intercepts <- c(
  set_prior("normal(-2, 1)", class = "Intercept", dpar = "mudirhe"),
  set_prior("normal(-2, 1)", class = "Intercept", dpar = "muvc")
)

# SECTION 2: Models

# Best model so far: cmpl_anim, cmpl_det, cmpl_complex, cmpl_indiv, verse_genre, era_style + hierarchical part

# Models to train, removing one predictor from the "best model":

# m5_anim_removed: cmpl_det, cmpl_complex, cmpl_indiv, verse_genre, era_style + hierarchical part


pri_m5_anim_removed <- c(
  pri_cmpl_complex,
  pri_cmpl_det,
  pri_cmpl_indiv,
  pri_verse_genre,
  pri_era_style,
  pri_intercepts
)

priors_m5_anim_removed <- c(pri_m5_anim_removed, pri_re)

m5_anim_removed <- brm(
  cmpl_constr ~ cmpl_complex + cmpl_det + cmpl_indiv +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_m5_anim_removed,
  chains = 4,
  iter = 8000,
  warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(m5_anim_removed, file = "models/models_for_bayes_factor_analysis/m5_anim_removed.rds")




# m5_det_removed: cmpl_anim, cmpl_complex, cmpl_indiv, verse_genre, era_style + hierarchical part


pri_m5_det_removed <- c(
  pri_cmpl_anim,
  pri_cmpl_complex,
  pri_cmpl_indiv,
  pri_verse_genre,
  pri_era_style,
  pri_intercepts
)

priors_m5_det_removed <- c(pri_m5_det_removed, pri_re)

m5_det_removed <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_complex + cmpl_indiv +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_m5_det_removed,
  chains = 4,
  iter = 8000,
  warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(m5_det_removed, file = "models/models_for_bayes_factor_analysis/m5_det_removed.rds")




# m5_complex_removed: cmpl_anim, cmpl_det, cmpl_indiv, verse_genre, era_style + hierarchical part

pri_m5_complex_removed <- c(
  pri_cmpl_anim,
  pri_cmpl_det,
  pri_cmpl_indiv,
  pri_verse_genre,
  pri_era_style,
  pri_intercepts
)

priors_m5_complex_removed <- c(pri_m5_complex_removed, pri_re)

m5_complex_removed <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_indiv +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_m5_complex_removed,
  chains = 4,
  iter = 8000,
  warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(m5_complex_removed, file = "models/models_for_bayes_factor_analysis/m5_complex_removed.rds")





# m5_indiv_removed: cmpl_anim, cmpl_det, cmpl_complex, verse_genre, era_style + hierarchical part


pri_m5_indiv_removed <- c(
  pri_cmpl_anim,
  pri_cmpl_det,
  pri_cmpl_complex,
  pri_verse_genre,
  pri_era_style,
  pri_intercepts
)

priors_m5_indiv_removed <- c(pri_m5_indiv_removed, pri_re)

m5_indiv_removed <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_m5_indiv_removed,
  chains = 4,
  iter = 8000,
  warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(m5_indiv_removed, file = "models/models_for_bayes_factor_analysis/m5_indiv_removed.rds")




# m5_verse_genre_removed: cmpl_anim, cmpl_det, cmpl_complex, cmpl_indiv, era_style + hierarchical part


pri_m5_verse_genre_removed <- c(
  pri_cmpl_anim,
  pri_cmpl_det,
  pri_cmpl_complex,
  pri_cmpl_indiv,
  pri_era_style,
  pri_intercepts
)

priors_m5_verse_genre_removed <- c(pri_m5_verse_genre_removed, pri_re)

m5_verse_genre_removed <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + cmpl_indiv +
    era_style +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_m5_verse_genre_removed,
  chains = 4,
  iter = 8000,
  warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(m5_verse_genre_removed, file = "models/models_for_bayes_factor_analysis/m5_verse_genre_removed.rds")




# m5_era_removed: cmpl_anim, cmpl_det, cmpl_complex, cmpl_indiv, verse_genre + hierarchical part


pri_m5_era_removed <- c(
  pri_cmpl_anim,
  pri_cmpl_det,
  pri_cmpl_complex,
  pri_cmpl_indiv,
  pri_verse_genre,
  pri_intercepts
)

priors_m5_era_removed <- c(pri_m5_era_removed, pri_re)

m5_era_removed <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + cmpl_indiv +
    verse_genre +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_m5_era_removed,
  chains = 4,
  iter = 8000,
  warmup = 4000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000,
  save_pars = save_pars(all = TRUE)
)

saveRDS(m5_era_removed, file = "models/models_for_bayes_factor_analysis/m5_era_removed.rds")


