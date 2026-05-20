# Load packages
library(dplyr)

# Load dataset

df <- readr::read_csv("data/filtered_dataset.csv", show_col_types = FALSE)

df

# How many goals of each type with examples

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

# Exploring the verbal lexemes (lex)

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