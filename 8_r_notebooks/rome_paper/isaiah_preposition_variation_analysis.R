library(dplyr)
library(tidyr)

isaiah_dataset <- read.csv(
  "data/isaiah_dataset.csv",
  stringsAsFactors = FALSE
)

dim(isaiah_dataset)
names(isaiah_dataset)
View(isaiah_dataset)

# check present manuscripts

unique(isaiah_dataset$book_scroll)

# restrict dataset to prepositional goals
prep_goals <- isaiah_dataset |>
  filter(cmpl_syntax == "prepositional")


# view info

prep_goals_check <- prep_goals |>
  select(
    book_scroll,
    chapter,
    verse_num,
    lex,
    verb_id,
    preposition_1,
    cmpl_lex,
    gcons_clause
  ) |>
  arrange(
    chapter,
    verse_num,
    lex,
    book_scroll,
    verb_id
  )

View(prep_goals_check)

# idenfity candidate shared by at least 2 manuscripts
shared_prep_candidates <- prep_goals |>
  group_by(chapter, verse_num, lex) |>
  filter(n_distinct(book_scroll) >= 2) |>
  ungroup() |>
  select(
    book_scroll,
    chapter,
    verse_num,
    lex,
    verb_id,
    preposition_1,
    cmpl_lex,
    gcons_clause
  ) |>
  arrange(
    chapter,
    verse_num,
    lex,
    book_scroll,
    verb_id
  )

View(shared_prep_candidates)

# Count: goals per manuscripts for each pair
prep_counts <- shared_prep_candidates |>
  count(chapter, verse_num, lex, book_scroll, name = "n_goals")

prep_counts_wide <- prep_counts |>
  tidyr::pivot_wider(
    names_from = book_scroll,
    values_from = n_goals,
    values_fill = 0
  )

View(prep_counts_wide)

# calculate the maximum shared number of goals for each pair

pairwise_shared_prep <- prep_counts_wide |>
  summarise(
    MT_1Qisaa = sum(pmin(Isaiah_MT, Isaiah_1Qisaa)),
    MT_1Q8 = sum(pmin(Isaiah_MT, Isaiah_1Q8)),
    `1Qisaa_1Q8` = sum(pmin(Isaiah_1Qisaa, Isaiah_1Q8))
  )

pairwise_shared_prep

# manually check ambiguous groups

ambiguous_prep_groups <- prep_counts_wide |>
  filter(
    Isaiah_MT > 1 |
      Isaiah_1Qisaa > 1 |
      Isaiah_1Q8 > 1
  ) |>
  arrange(chapter, verse_num, lex)

View(ambiguous_prep_groups)

# see ambiguous cases

ambiguous_prep_details |>
  select(
    book_scroll,
    chapter,
    verse_num,
    lex,
    verb_id,
    preposition_1,
    cmpl_lex,
    gcons_clause
  ) |>
  arrange(
    chapter,
    verse_num,
    lex,
    book_scroll,
    verb_id
  ) |>
  print(n = Inf)

View(ambiguous_prep_details)

# view disagreements

prep_disagreement_candidates <- prep_goals |>
  group_by(chapter, verse_num, lex) |>
  filter(n_distinct(book_scroll) >= 2) |>
  filter(n_distinct(preposition_1) > 1) |>
  ungroup() |>
  select(
    book_scroll,
    chapter,
    verse_num,
    lex,
    verb_id,
    preposition_1,
    cmpl_lex,
    gcons_clause
  ) |>
  arrange(
    chapter,
    verse_num,
    lex,
    book_scroll,
    verb_id
  )

prep_disagreement_candidates |>
  print(n = Inf)