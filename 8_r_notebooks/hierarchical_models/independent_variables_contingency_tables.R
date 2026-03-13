# Packages
library(tidyverse)
library(janitor)

# 1) Load data
df <- readr::read_csv("data/filtered_dataset.csv", show_col_types = FALSE) %>%
  clean_names()

# 2) Independent variables (iv) to check
ivs <- c(
  "cmpl_anim",
  "cmpl_det",
  "cmpl_complex",
  "motion_type",
  "verse_genre",
  "era_style"
)

# 3) Generate all unique IV pairs
pairs <- combn(ivs, 2, simplify = FALSE)

# 4) write the function to get the observed counts and get the counts

## Function
get_observed_expected <- function(df, v1, v2) {
  
  # Observed counts
  observed <- table(df[[v1]], df[[v2]])
  
  # Chi-square test object (we only use it to get expected counts)
  chisq_obj <- suppressWarnings(chisq.test(observed, correct = FALSE))
  
  expected <- chisq_obj$expected
  
  list(
    var1 = v1,
    var2 = v2,
    observed = observed,
    expected = expected
  )
}

## Observed counts

obs_exp_tables <- purrr::map(
  pairs,
  \(p) get_observed_expected(df, p[1], p[2])
)

print(obs_exp_tables)

## Example 1: verse_genre × era_style
verse_genre_and_era_style <- obs_exp_tables %>%
  purrr::keep(~ .x$var1 == "verse_genre" & .x$var2 == "era_style") %>%
  purrr::pluck(1)

## Observed counts
verse_genre_and_era_style$observed

## Expected counts
round(verse_genre_and_era_style$expected, 2)

## Example 2: motion_type x verse_genre
motion_type_and_verse_genre <- obs_exp_tables %>%
  purrr::keep(~ .x$var1 == "motion_type" & .x$var2 == "verse_genre") %>%
  purrr::pluck(1)

## Observed counts
motion_type_and_verse_genre$observed

## Expected counts
round(motion_type_and_verse_genre$expected, 2)

## Example 3: cmpl_anim × cmpl_complex
cmpl_anim_and_cmpl_complex <- obs_exp_tables %>%
  purrr::keep(~ .x$var1 == "cmpl_anim" & .x$var2 == "cmpl_complex") %>%
  purrr::pluck(1)

## Observed counts
cmpl_anim_and_cmpl_complex$observed

## Expected counts
round(cmpl_anim_and_cmpl_complex$expected, 2)

## Example 4: cmpl_det × verse_genre
cmpl_anim_and_verse_genre <- obs_exp_tables %>%
  purrr::keep(~ .x$var1 == "cmpl_det" & .x$var2 == "verse_genre") %>%
  purrr::pluck(1)

## Observed counts
cmpl_anim_and_verse_genre$observed

## Expected counts
round(cmpl_anim_and_verse_genre$expected, 2)

## Example 5: cmpl_det × era_style
cmpl_anim_and_era_style <- obs_exp_tables %>%
  purrr::keep(~ .x$var1 == "cmpl_det" & .x$var2 == "era_style") %>%
  purrr::pluck(1)

## Observed counts
cmpl_anim_and_era_style$observed

## Expected counts
round(cmpl_anim_and_era_style$expected, 2)


# 5) make a tidy table

## Function
make_tidy_table <- function(res) {
  
  obs <- as.data.frame(as.table(res$observed), stringsAsFactors = FALSE)
  exp <- as.data.frame(as.table(res$expected), stringsAsFactors = FALSE)
  
  colnames(obs) <- c("level_var1", "level_var2", "observed")
  colnames(exp) <- c("level_var1", "level_var2", "expected")
  
  obs %>%
    mutate(
      level_var1 = as.character(level_var1),
      level_var2 = as.character(level_var2)
    ) %>%
    left_join(
      exp %>%
        mutate(
          level_var1 = as.character(level_var1),
          level_var2 = as.character(level_var2)
        ),
      by = c("level_var1", "level_var2")
    ) %>%
    mutate(
      var1 = res$var1,
      var2 = res$var2,
      diff = observed - expected
    ) %>%
    select(var1, var2, level_var1, level_var2, observed, expected, diff)
}

tidy_example <- make_tidy_table(cmpl_anim_and_era_style)
print(cmpl_anim_and_era_style)