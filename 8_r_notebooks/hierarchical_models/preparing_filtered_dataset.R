### Load packages
library(janitor)
library(tidyverse)

set.seed(84735)


### Loading files and filtering

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


### Look at the data
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