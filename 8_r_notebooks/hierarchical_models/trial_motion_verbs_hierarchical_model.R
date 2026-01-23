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
  set_prior("normal(0, 1.5)", class="b", coef="verse_genreQumran", dpar="mudirhe"),
  set_prior("normal(0, 1.5)", class="b", coef="verse_genreQumran", dpar="muvc"),
  
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
  set_prior("student_t(3, 0, 1)", class = "sd", group = "book_scroll", coef = "Intercept", dpar = dp),
  set_prior("student_t(3, 0, 1)", class = "sd", group = "lex", coef = "Intercept", dpar = dp)
)))

priors <- c(pri_manual, pri_re)
priors

### Prior predictive check
# The prior predictive check is used to compute what kind of plausible data
# we would obtain if the data was fully governed by the determined priors
# the prior predictive check does not check for model fit 
# and does not compare to the observed data

prior_only_model <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors,
  sample_prior = "only",
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.8),
  refresh = 0
)

## Visualize prior predictive check

# Category intercepts (dependent variable)
pp_check(
  prior_only_model,
  type = "bars",
  ndraws = 2000,
)

# Have a look at the grouping variables SD implications

dr <- as_draws_df(prior_only_model)
# brms names SD parameters like: sd_group__Intercept (and include dpar for categorical)
sd_names <- grep("^sd_", names(dr), value = TRUE)
sd_names

summ_sd <- function(x) {
  quantile(x, probs = c(.5, .8, .9, .95, .99))
}

setNames(lapply(sd_names, \(nm) summ_sd(dr[[nm]])), sd_names)


### Hierarchical model - book_scroll and verb as crossed factors

### Model 1 - BOok_scroll / lex as crossed factors

# model code

#TODO: correlation between the independent variables

motion_verb_1 <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | lex), # crossed factors
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors,
  chains = 4,
  iter = 8000, 
  warmup = 4000,
  seed = 84735,
  init = "0",                        # fixes many init failures
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  refresh = 1000
)

summary(motion_verb_1)

# save the fitted model to a file
saveRDS(motion_verb_1, file = "models/motion_verb_1.rds")


### Model 2 - Book_scroll / lex as nested factors

### # Get priors for book_scroll and lexeme as nested factors

gp_2 <- get_prior(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | book_scroll:lex), # or (1 | book_scroll/lex), less explicit
  data = df0,
  family = categorical(link = "logit", refcat = "prep")
)

gp_2

# all distributional parameters (one per non-reference category)
dpars_2 <- unique(gp_2$dpar[gp_2$class %in% c("b","Intercept") & nzchar(gp_2$dpar)])

# Group-Level parameters' priors, random-intercept standard deviation (sd)
pri_re_2 <- do.call(c, lapply(dpars_2, function(dp) c(
  set_prior("student_t(3, 0, 1)", class = "sd", group = "book_scroll", coef = "Intercept", dpar = dp),
  set_prior("student_t(3, 0, 1)", class = "sd", group = "book_scroll:lex", coef = "Intercept", dpar = dp)
)))


priors_2 <- c(pri_manual, pri_re_2) # I use the same priors for the fixed effects as for model 1
priors_2

### Prior predictive check

prior_only_model_2 <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | book_scroll:lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_2,
  sample_prior = "only",
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 84735,
  init = "0",
  control = list(adapt_delta = 0.8),
  refresh = 0
)

## Visualize prior predictive check

# Category intercepts (dependent variable)
pp_check(
  prior_only_model_2,
  type = "bars",
  ndraws = 2000
)

# Have a look at the grouping variables SD implications

dr_2 <- as_draws_df(prior_only_model_2)
# brms names SD parameters like: sd_group__Intercept (and include dpar for categorical)
sd_names_2 <- grep("^sd_", names(dr_2), value = TRUE)
sd_names_2

summ_sd <- function(x) {
  quantile(x, probs = c(.5, .8, .9, .95, .99))
}

setNames(lapply(sd_names_2, \(nm) summ_sd(dr_2[[nm]])), sd_names_2)

# model code
motion_verb_2_nested <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    verse_genre + era_style +
    (1 | book_scroll) + (1 | book_scroll:lex), # or (1 | book_scroll/lex) but less explicit
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors_2,
  chains = 4,
  iter = 8000, 
  warmup = 4000,
  seed = 84735,
  init = "0",                        # fixes many init failures
  control = list(adapt_delta = 0.9, max_treedepth = 12), # increased to fixe divergent transitions
  refresh = 1000
)

summary(motion_verb_2_nested)

# Notes adapt_delta = 0.8 and max_treedepth = 10 ==> 15 divergent transitions

# save the fitted model to a file
saveRDS(motion_verb_2_nested, file = "models/motion_verb_2_nested.rds")

# Debug model: works

m_dbg <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior = priors,
  chains = 2,
  iter = 1000, warmup = 500,
  seed = 84735,
  init  = "0",                        # <- crucial: fixes many init failures
  control = list(adapt_delta = 0.995, max_treedepth = 15),
  refresh = 250
)