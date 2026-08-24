# Isaiah manuscript analysis

# Clear the environment
rm(list = ls())

# packages
library(dplyr)
library(tidyr)

# Read the Isaiah dataset
isaiah_dataset <- read.csv(
  "data/isaiah_dataset.csv",
  stringsAsFactors = FALSE
)

# Quick checks
# Inspect the dataset
dim(isaiah_dataset)
names(isaiah_dataset)
head(isaiah_dataset)

# Create a function that allows an easy comparison of a specific column

compare_manuscripts <- function(data, variable) {
  
  # Create one signature per manuscript and verse
  signatures <- data |>
    group_by(book_scroll, chapter, verse_num) |>
    summarise(
      signature = paste(sort(.data[[variable]]), collapse = " || "),
      .groups = "drop"
    ) |>
    rename(!!variable := signature)
  
  # Find verses where manuscripts disagree
  disagreements <- signatures |>
    group_by(chapter, verse_num) |>
    summarise(
      manuscripts = n(),
      different_signatures = n_distinct(.data[[variable]]),
      .groups = "drop"
    ) |>
    filter(
      manuscripts > 1,
      different_signatures > 1
    )
  
  # Return the details
  signatures |>
    inner_join(
      disagreements,
      by = c("chapter", "verse_num")
    ) |>
    arrange(chapter, verse_num, book_scroll)
}

# in Isaiah dataset, compare "cmpl_syntax"

View(compare_manuscripts(isaiah_dataset, "cmpl_syntax"))

# in Isaiah dataset, compare "gcons_clause"
View(compare_manuscripts(isaiah_dataset, "cmpl_lex"))


# Number of verbal occurrences in each book_scroll

isaiah_dataset |>
  summarise(
    n_verbs = n_distinct(verb_id),
    .by = book_scroll
  ) |>
  mutate(
    percentage = round(n_verbs / sum(n_verbs) * 100, 1)
  )


# Comparing occurrences in MT and 1QIsaa

# comparison dataset

mt_1qisaa <- isaiah_dataset |>
  filter(book_scroll %in% c("Isaiah_MT", "Isaiah_1Qisaa"))

# create occurrence check for counting the row combinations

occurrence_check <- mt_1qisaa |>
  group_by(book_scroll, chapter, verse_num, lex) |>
  summarise(
    n_goals = n(),
    .groups = "drop"
  )

# create readable table

occurrence_comparison <- occurrence_check |>
  pivot_wider(
    names_from = book_scroll,
    values_from = n_goals,
    values_fill = 0
  ) |>
  arrange(chapter, verse_num, lex)

View(occurrence_comparison)

# for manual checking

manual_check <- mt_1qisaa |>
  select(
    book_scroll,
    chapter,
    verse_num,
    lex,
    cmpl_constr,
    gcons_clause
  ) |>
  arrange(
    chapter,
    verse_num,
    lex,
    book_scroll,
  )

View(manual_check)