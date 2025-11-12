# Load packages
library(dplyr)
library(forcats)
library(bayesrules)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(janitor)
library(tidybayes)
library(broom.mixed)
library(brms)
set.seed(84735)

options(mc.cores = 2)


file_path <- "data/combined_datasets.csv"
df_raw <- readr::read_csv(file_path, show_col_types=FALSE)

# Clean column names to be simple (lowercase, underscores)
df <- df_raw %>% clean_names()

# Drop rows where complement == "no complement"
df <- df %>%
  filter(complement != "no complement" | is.na(complement)) %>%
  mutate(cmpl_constr = forcats::fct_drop(cmpl_constr))

# Drop rows with reconstructed occurrences, min, not predicate
df <- df %>%
  filter(!(comments %in% c("reconstructed", "verb rec", "reconstructed?", "not predicate", "min excluded")) | is.na(comments)) %>%
  mutate(cmpl_constr = forcats::fct_drop(cmpl_constr))
df %>% count(comments, sort = TRUE)

# Keep only goals (motion_type)
df <- df %>%
  filter(spatial_arg_type == "goal") %>%
  mutate(cmpl_constr = forcats::fct_drop(cmpl_constr))
df %>% count(spatial_arg_type, sort = TRUE)

# Keep only horizontal goals (motion_type), excluding vertical and posture/not motion
df <- df %>%
  filter(!(motion_type %in% c("vertical", "posture/not motion")) | is.na(motion_type)) %>%
  mutate(cmpl_constr = forcats::fct_drop(cmpl_constr))
df %>% count(motion_type, sort = TRUE)


# Look at the data
glimpse(df)

# Print categories of dependent variable
levels(df$cmpl_constr)


# Create book_scroll (even if one of the parts is missing)
df <- df %>%
  mutate(
    book = as.character(book),
    scroll = as.character(scroll),
    book_scroll = ifelse(is.na(book) & is.na(scroll), NA_character_,
                         ifelse(is.na(book), paste0("NA_", scroll),
                                ifelse(is.na(scroll), paste0(book, "_NA"), paste(book, scroll, sep = "_"))))
  )

# Create a cmpl_syntax column to try out the model with stan_glmer

df <- df %>%
  mutate(
    cmpl_syntax = case_when(
      cmpl_constr %in% c("prep", "prep + dir-he") ~ "prepositional",
      cmpl_constr %in% c("dir-he", "vc") ~ "non-prepositional",
      TRUE ~ NA_character_   # fallback in case of unexpected value
    ),
    cmpl_syntax = factor(cmpl_syntax, levels = c("prepositional", "non-prepositional"))
  )
df %>% count(cmpl_constr, cmpl_syntax)
levels(df$cmpl_syntax)
nlevels(df$cmpl_syntax)

# Save the filtered dataset
readr::write_csv(df, "data/filtered_dataset.csv")
list.files("data")

View(df)

# Coerce relevant fields to factors
df <- df %>%
  mutate(
    cmpl_constr   = as.factor(cmpl_constr),
    lex           = as.factor(lex),            
    book_scroll   = as.factor(book_scroll),    
    cmpl_anim     = as.factor(cmpl_anim),
    cmpl_det      = as.factor(cmpl_det),
    cmpl_indiv    = as.factor(cmpl_indiv),
    cmpl_complex  = as.factor(cmpl_complex),
    motion_type   = as.factor(motion_type)
  )

# Basic sanity: drop rows missing key fields
df <- df %>%
  filter(!is.na(cmpl_constr), !is.na(lex), !is.na(book_scroll))


# Number of book_scrolls and lexemes
nlevels(df$book_scroll)
nlevels(df$lex)

df %>% summarize(
  n_book_scroll   = nlevels(book_scroll),
  n_lex    = nlevels(lex)
)

# Checks on the data

# Confirm binary variable (dependent variable)

df_chk <- df %>%
  mutate(
    cmpl_syntax = fct_drop(as.factor(cmpl_syntax)),
    # make sure the first level is the "success" you want (optional)
    cmpl_syntax = fct_relevel(cmpl_syntax, "prepositional")
  )

levels(df_chk$cmpl_syntax)
df_chk %>% count(cmpl_syntax)

# Remove NAs and drop unused levels

df_chk <- df_chk %>%
  filter(!is.na(cmpl_syntax), !is.na(book_scroll), !is.na(lex),
         !is.na(cmpl_anim), !is.na(cmpl_det), !is.na(cmpl_complex), !is.na(cmpl_indiv), !is.na(motion_type)) %>%
  mutate(
    book_scroll   = fct_drop(as.factor(book_scroll)),
    lex           = fct_drop(as.factor(lex)),
    cmpl_anim     = fct_drop(as.factor(cmpl_anim)),
    cmpl_det      = fct_drop(as.factor(cmpl_det)),
    cmpl_complex  = fct_drop(as.factor(cmpl_complex)),
    cmpl_indiv    = fct_drop(as.factor(cmpl_indiv)),
    motion_type   = fct_drop(as.factor(motion_type))
  )

# sanity
sapply(df_chk[c("book_scroll","lex","cmpl_anim","cmpl_det","cmpl_complex","cmpl_indiv","motion_type")], nlevels)

# complete separation (a predictor that appear only with one outcome)

# helper to flag levels that are all one class
flag_sep <- function(var){
  df_chk %>% count({{var}}, cmpl_syntax) %>%
    tidyr::pivot_wider(names_from = cmpl_syntax, values_from = n, values_fill = 0) %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    filter(prepositional == 0 | `non-prepositional` == 0) %>%
    arrange(desc(total))
}

flag_sep(cmpl_anim)
flag_sep(cmpl_det)
flag_sep(cmpl_complex)
flag_sep(cmpl_indiv)
flag_sep(motion_type)

# Show problematic rows of comp_indiv

problem_levels <- df %>%
  count(cmpl_indiv, cmpl_syntax) %>%
  pivot_wider(names_from = cmpl_syntax, values_from = n, values_fill = 0) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  filter(prepositional == 0 | `non-prepositional` == 0) %>%
  pull(cmpl_indiv)

problem_levels

# View problematic rows 
View(df %>% filter(cmpl_indiv %in% problem_levels))

# NB: for now, remove cmpl_indiv from the models

# Choose baseline categories

levels(df$cmpl_constr)
levels(df$cmpl_det)
levels(df$cmpl_anim)

# Dependent variable
df$cmpl_constr <- relevel(df$cmpl_constr, ref = "prep")

# Individual predictor variables
df$cmpl_anim    <- relevel(df$cmpl_anim, ref = "inanim")
df$cmpl_det     <- relevel(df$cmpl_det,  ref = "det")
df$cmpl_complex <- relevel(df$cmpl_complex, ref = "simple")
df$motion_type  <- relevel(df$motion_type, ref = "factive")

# Define priors

gp <- get_prior(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    (1 | book_scroll) + (1 | lex),
  data = df,
  family = categorical(link = "logit", refcat = "prep")
)

# all distributional parameters (one per non-reference category)
dpars <- unique(gp$dpar[gp$class %in% c("b","Intercept") & nzchar(gp$dpar)])

# fixed-effect priors for every category-specific linear predictor
pri_fixed <- do.call(c, lapply(dpars, function(dp) c(
  set_prior("normal(0, 2)",   class = "b",        dpar = dp),
  set_prior("normal(0, 2.5)", class = "Intercept", dpar = dp)
)))

# random-intercept SDs (per category & per grouping factor)
pri_re <- do.call(c, lapply(dpars, function(dp) c(
  set_prior("student_t(3, 0, 2.5)", class = "sd", group = "book_scroll", coef = "Intercept", dpar = dp),
  set_prior("student_t(3, 0, 2.5)", class = "sd", group = "lex",         coef = "Intercept", dpar = dp)
)))

priors <- c(pri_fixed, pri_re)
priors

# Sanity checks

# outcome actually has all intended levels and nonzero counts?
table(df$cmpl_constr, useNA = "ifany")

# Hierarchical model - book_scroll and verb as crossed factors

# Using a df without empty levels
df0 <- droplevels(df)

motion_verb_1 <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    (1 | book_scroll) + (1 | lex),
  data = df,
  family = categorical(link = "logit", refcat = "prep"),
  prior  = priors,
  chains = 1, 
  iter = 4000, 
  warmup = 2000,
  cores = 4,
  seed = 84735,
  control = list(adapt_delta = 0.8, maxtreedepth = 12),
  refresh = 200, 
  silent = 0
)

# Debug model

m_dbg <- brm(
  cmpl_constr ~ cmpl_anim + cmpl_det + cmpl_complex + motion_type +
    (1 | book_scroll) + (1 | lex),
  data = df0,
  family = categorical(link = "logit", refcat = "prep"),
  prior   = priors,
  chains  = 1,
  iter    = 1500, warmup = 1000,
  seed    = 84735,
  init   = "0",                        # <- crucial: fixes many init failures
  control = list(adapt_delta = 0.995, max_treedepth = 15),
  refresh = 50
)