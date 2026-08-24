######################## Load packages #######################
library(dplyr)
library(tidyr)

######################## Load dataset #######################

df <- readr::read_csv("data/filtered_dataset.csv", show_col_types = FALSE)

df

####################### How many goals of each type with examples #######################

goal_construction_counts <- df %>%
  count(cmpl_constr, name = "n") %>%
  arrange(desc(n))

goal_construction_counts

goal_construction_counts <- df %>%
  count(cmpl_constr, name = "n") %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

goal_construction_counts

# Extract examples

goal_construction_examples <- df %>%
  group_by(cmpl_constr) %>%
  slice_sample(n = 10) %>%
  select(
    cmpl_constr,
    lex,
    book_scroll,
    chapter,
    verse_num,
    complement,
    cmpl_translation,
    motion_type,
    spatial_arg_type
  ) %>%
  arrange(cmpl_constr)

print(goal_construction_examples, n = Inf)
View(goal_construction_examples)

####################### Exploring the verbal lexemes (lex) #######################

## All the lexemes

### Table

all_lexemes <- df %>%
  count(lex, name = "n") %>%
  arrange(desc(n))

View(all_lexemes)

### How many different lexemes

n_distinct_lex <- n_distinct(df$lex)

n_distinct_lex

## Ten most frequent lexemes

lex_counts <- df %>%
  count(lex, name = "n") %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

top_10_lex <- lex_counts %>%
  slice_head(n = 10)

top_10_lex

View(top_10_lex)

## Explore a specific lexeme

### Define lexeme

lexeme <- "NWS["

### Get 10 examples of that lexeme

lexeme_examples <- df %>%
  filter(lex == lexeme) %>%
  slice_sample(n = 13) %>%
  select(
    lex,
    book_scroll,
    chapter,
    verse_num,
    complement,
    cmpl_translation,
    cmpl_constr,
    motion_type,
    spatial_arg_type
  )

print(lexeme_examples, n = Inf)

View(lexeme_examples)


####################### Exploring the prepositions (preposition_1) #######################

# Rows with a prepositional complement construction

prep_df <- df %>%
  filter(grepl("prep", cmpl_constr)) %>%
  select(
    lex,
    book_scroll,
    chapter,
    verse_num,
    complement,
    cmpl_translation,
    cmpl_constr,
    motion_type,
    preposition_1,
    preposition_2
  )

# Count prepositions

prep_counts <- prep_df %>%
  count(preposition_1, name = "n") %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

View(prep_counts)

# How many different prepositions are found
n_distinct_prepositions <- prep_df %>%
  summarise(n_distinct_prepositions = n_distinct(preposition_1, na.rm = TRUE))

n_distinct_prepositions

# Examples

prep_examples <- prep_df %>%
  group_by(preposition_1) %>%
  slice_head(n = 10) %>%
  arrange(preposition_1)

View(prep_examples)




######################## Goal characteristics #######################

# Goal animacy
df %>%
  count(cmpl_anim, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Goal complexity
df %>%
  count(cmpl_complex, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Goal definiteness
df %>%
  count(cmpl_det, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Goal individuation
df %>%
  count(cmpl_indiv, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

######################## Combined table of goal characteristics #######################


goal_characteristics <- df %>%
  
  # Select the variables of interest
  select(cmpl_anim, cmpl_complex, cmpl_det, cmpl_indiv) %>%
  
  # Reshape to long format
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  
  # Count categories
  count(Variable, Category) %>%
  
  # Calculate percentages within each variable
  group_by(Variable) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 1)
  ) %>%
  
  # Optional: rename columns for readability
  rename(
    Count = n
  ) %>%
  
  ungroup()

# Display table
View(goal_characteristics)

goal_characteristics



## Quick verif: good number of occurrences

verification_table <- df %>%
  
  select(cmpl_anim, cmpl_complex, cmpl_det, cmpl_indiv) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  
  group_by(Variable) %>%
  summarise(
    Total = n(),
    Missing = sum(is.na(Category)),
    Non_missing = sum(!is.na(Category))
  )

View(verification_table)

verification_table

######################## Combined table of textual characteristics #######################


textual_characteristics <- df %>%
  
  # Select the variables of interest
  select(era_style, verse_genre) %>%
  
  # Reshape to long format
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Category"
  ) %>%
  
  # Count categories
  count(Variable, Category) %>%
  
  # Calculate percentages within each variable
  group_by(Variable) %>%
  mutate(
    Percent = round(n / sum(n) * 100, 1)
  ) %>%
  
  # Optional: rename columns for readability
  rename(
    Count = n
  ) %>%
  
  ungroup()

# Display table
View(textual_characteristics)

textual_characteristics