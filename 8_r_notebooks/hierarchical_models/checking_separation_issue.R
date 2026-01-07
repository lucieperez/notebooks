# 03_check_separation.R
# Check (quasi-)complete separation in the binary outcome cmpl_syntax
# using the already-filtered dataset: data/filtered_dataset.csv

library(tidyverse)
library(forcats)

# ---- Load filtered data ----
df <- readr::read_csv("data/filtered_dataset.csv", show_col_types = FALSE)

# ---- Coerce to factors and construct cmpl_syntax ----
df <- df %>%
  mutate(
    cmpl_constr   = as.factor(cmpl_constr),
    lex           = as.factor(lex),
    book_scroll   = as.factor(book_scroll),
    cmpl_anim     = as.factor(cmpl_anim),
    cmpl_det      = as.factor(cmpl_det),
    cmpl_indiv    = as.factor(cmpl_indiv),
    cmpl_complex  = as.factor(cmpl_complex),
    motion_type   = as.factor(motion_type),
    
    # Binary outcome: prepositional vs non-prepositional
    cmpl_syntax = case_when(
      cmpl_constr %in% c("prep", "prep + dir-he") ~ "prepositional",
      cmpl_constr %in% c("dir-he", "vc")          ~ "non-prepositional",
      TRUE                                        ~ NA_character_
    ),
    cmpl_syntax = factor(cmpl_syntax,
                         levels = c("prepositional", "non-prepositional"))
  )

# ---- Build analysis dataframe for separation checks ----
df_chk <- df %>%
  filter(
    !is.na(cmpl_syntax),
    !is.na(book_scroll),
    !is.na(lex),
    !is.na(cmpl_anim),
    !is.na(cmpl_det),
    !is.na(cmpl_complex),
    !is.na(cmpl_indiv),
    !is.na(motion_type)
  ) %>%
  mutate(
    book_scroll   = fct_drop(book_scroll),
    lex           = fct_drop(lex),
    cmpl_anim     = fct_drop(cmpl_anim),
    cmpl_det      = fct_drop(cmpl_det),
    cmpl_complex  = fct_drop(cmpl_complex),
    cmpl_indiv    = fct_drop(cmpl_indiv),
    motion_type   = fct_drop(motion_type),
    cmpl_syntax   = fct_relevel(cmpl_syntax, "prepositional")
  )

# Quick sanity check (optional)
print(table(df_chk$cmpl_syntax))

# ---- Helper to flag separation ----
# For a given factor, returns levels that occur with only one outcome class
flag_sep <- function(var) {
  df_chk %>%
    count({{ var }}, cmpl_syntax) %>%
    tidyr::pivot_wider(
      names_from  = cmpl_syntax,
      values_from = n,
      values_fill = 0
    ) %>%
    mutate(total = rowSums(across(where(is.numeric)))) %>%
    filter(prepositional == 0 | `non-prepositional` == 0) %>%
    arrange(desc(total))
}

# ---- Run separation checks for each predictor ----
sep_cmpl_anim    <- flag_sep(cmpl_anim)
sep_cmpl_det     <- flag_sep(cmpl_det)
sep_cmpl_complex <- flag_sep(cmpl_complex)
sep_cmpl_indiv   <- flag_sep(cmpl_indiv)
sep_motion_type  <- flag_sep(motion_type)

cat("\n=== Separation: cmpl_anim ===\n")
print(sep_cmpl_anim)

cat("\n=== Separation: cmpl_det ===\n")
print(sep_cmpl_det)

cat("\n=== Separation: cmpl_complex ===\n")
print(sep_cmpl_complex)

cat("\n=== Separation: cmpl_indiv ===\n")
print(sep_cmpl_indiv)

cat("\n=== Separation: motion_type ===\n")
print(sep_motion_type)

# ---- Inspect problematic cmpl_indiv levels more closely ----
problem_levels_cmpl_indiv <- df_chk %>%
  count(cmpl_indiv, cmpl_syntax) %>%
  tidyr::pivot_wider(
    names_from  = cmpl_syntax,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(total = rowSums(across(where(is.numeric)))) %>%
  filter(prepositional == 0 | `non-prepositional` == 0) %>%
  pull(cmpl_indiv)

cat("\n=== Problematic cmpl_indiv levels (only one outcome class) ===\n")
print(problem_levels_cmpl_indiv)

problem_rows_cmpl_indiv <- df_chk %>%
  filter(cmpl_indiv %in% problem_levels_cmpl_indiv)

# To inspect interactively in RStudio, uncomment:
# View(problem_rows_cmpl_indiv)

# Or just print a summary:
cat("\n=== Example rows for problematic cmpl_indiv levels ===\n")
print(problem_rows_cmpl_indiv %>% select(cmpl_indiv, cmpl_syntax, everything()) %>% head())
