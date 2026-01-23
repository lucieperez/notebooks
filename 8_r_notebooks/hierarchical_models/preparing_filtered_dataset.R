### Load packages
library(janitor)
library(tidyverse)

set.seed(84735)


### Loading files and filtering

file_path <- "data/dataset_with_genre_era.csv"
df_raw <- readr::read_csv(file_path, show_col_types=FALSE)

# Clean column names to be simple (lowercase, underscores)
df <- df_raw %>% clean_names()

# Drop rows where complement == "no complement"
df <- df %>%
  filter(complement != "no complement" | is.na(complement)) %>%
  mutate(cmpl_constr = forcats::fct_drop(cmpl_constr))

# Keep only Hebrew verses
df <- df %>%
  filter(verse_language == "Hebrew")

# Recode era_style: TBH and debated -> other
df <- df %>%
  mutate(
    era_style = case_when(
      era_style %in% c("TBH", "debated") ~ "other",
      TRUE ~ era_style
    )
  )

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
  filter(!(motion_type %in% c("vertical", "posture/not motion", "metaphor")) | is.na(motion_type)) %>%
  mutate(cmpl_constr = forcats::fct_drop(cmpl_constr))
df %>% count(motion_type, sort = TRUE)

# Remove the sham cases ==> cmpl_lex == "CM" or "CM="
df <- df %>%
  filter(!(cmpl_lex %in% c("CM", "CM=")) | is.na(cmpl_lex))

df <- df %>%
  mutate(cmpl_lex = forcats::fct_drop(cmpl_lex))

df %>%
  filter(cmpl_lex %in% c("CM", "CM="))

### Look at the data
glimpse(df)

# Print categories of dependent variable
levels(df$cmpl_constr)


# Create book_scroll (even if one of the parts is missing)
df <- df %>%
  mutate(
    book_canonical = as.character(book_canonical),
    scroll = as.character(scroll),
    book_scroll = ifelse(is.na(book_canonical) & is.na(scroll), NA_character_,
                         ifelse(is.na(book_canonical), paste0("NA_", scroll),
                                ifelse(is.na(scroll), paste0(book_canonical, "_NA"), paste(book_canonical, scroll, sep = "_"))))
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

### Remove "prep + dir-he" cases entirely (only 7 cases, too small for being a category)
df <- df %>%
  filter(cmpl_constr != "prep + dir-he" | is.na(cmpl_constr)) %>%
  droplevels()



# Collapse "prep + prep" into "prep"
df <- df %>%
  mutate(
    cmpl_constr = as.character(cmpl_constr),
    cmpl_constr = if_else(cmpl_constr == "prep + prep", "prep", cmpl_constr),
    cmpl_constr = factor(cmpl_constr)
  ) %>%
  droplevels()



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
    era_style = as.factor(era_style),
  )

# Additional sanity checks after adding the new columns (genre, era)

df %>% count(verse_genre, sort = TRUE)
df %>% count(motion_type, sort = TRUE)
df %>% count(era_style, sort = TRUE)
df %>% count(book_canonical, sort = TRUE)
df %>% count(text_unit, sort = TRUE)

# Save the filtered dataset
readr::write_csv(df, "data/filtered_dataset.csv")
list.files("data")


View(df)