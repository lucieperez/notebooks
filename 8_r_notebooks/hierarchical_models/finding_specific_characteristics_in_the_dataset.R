######################## Load packages #######################
library(dplyr)
library(tidyr)

######################## Load dataset #######################

df <- readr::read_csv("data/filtered_dataset.csv", show_col_types = FALSE)

df

#
df$era_style

######################## Find examples by goal characteristics #######################

# Define desired values
target_anim <- "inanim"
target_complex <- NA
target_det <- NA
target_indiv <- "prsf"

target_era_style <- NA
target_verse_genre <- NA

# Find matching examples
matching_examples <- df %>%
  filter(
    if (is.na(target_anim)) TRUE else cmpl_anim == target_anim,
    if (is.na(target_complex)) TRUE else cmpl_complex == target_complex,
    if (is.na(target_det)) TRUE else cmpl_det == target_det,
    if (is.na(target_indiv)) TRUE else cmpl_indiv == target_indiv,
    if (is.na(target_era_style)) TRUE else era_style == target_era_style,
    if (is.na(target_verse_genre)) TRUE else verse_genre == target_verse_genre
  ) %>%
  select(
    cmpl_constr,
    lex,
    book_scroll,
    chapter,
    verse_num,
    complement,
    cmpl_translation,
    gcons_clause
  )

# Display results
if (nrow(matching_examples) == 0) {
  
  "No verse found"
  
} else {
  
  matching_examples %>%
    slice_sample(n = min(5, nrow(matching_examples)))
}
