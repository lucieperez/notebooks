# Packages
library(dplyr)
library(stringr)

# Load the dataset
filtered_dataset <- read.csv(
  "data/filtered_dataset.csv",
  stringsAsFactors = FALSE
)

# Inspect the dataset
dim(filtered_dataset)
names(filtered_dataset)
head(filtered_dataset)

filtered_dataset |>
  distinct(spatial_arg_type)

# Book canonical Isaiah
filtered_dataset |>
  filter(book_canonical == "Isaiah") |>
  distinct(book_scroll)

# create the Isaiah dataset
isaiah_dataset <- filtered_dataset |>
  filter(book_canonical == "Isaiah")

# Number of rows and columns
dim(isaiah_dataset)

# Confirm that only the three Isaiah manuscripts are present
isaiah_dataset |>
  count(book_scroll)

# Confirm that every row belongs to canonical Isaiah
unique(isaiah_dataset$book_canonical)

# Save the new filtered Isaiah dataset
write.csv(
  isaiah_dataset,
  "data/isaiah_dataset.csv",
  row.names = FALSE
)


###### data exploration on filtered_dataset #####

mt_dataset <- filtered_dataset |>
  filter(scroll == "MT")

# check the filter

dim(mt_dataset)
unique(mt_dataset$scroll)

# Count distinct verb occurrences for each lexeme
mt_verb_counts <- mt_dataset |>
  group_by(lex) |>
  summarise(
    n_verbs = n_distinct(verb_id),
    .groups = "drop"
  )

# Keep lexemes occurring at least 20 times
mt_verbs_20 <- mt_verb_counts |>
  filter(n_verbs >= 20) |>
  arrange(desc(n_verbs))

# See the result
mt_verbs_20


##### 'ALAH in filtered_dataset, focused on scroll = "MT" #####

# filter the dataset
alah_mt <- filtered_dataset |>
  filter(
    scroll == "MT",
    lex == "<LH["
  )

# count occurrences

alah_mt |>
  summarise(
    n_verbs = n_distinct(verb_id)
  )

# count goal syntax
alah_mt |>
  count(cmpl_syntax, sort = TRUE) |>
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

# count goal construction
alah_mt |>
  count(cmpl_constr, sort = TRUE) |>
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

View(alah_mt)

# count book distribution
alah_mt |>
  count(book_canonical, sort = TRUE) |>
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

# counting the preposition_1

# filter the prepositional goals
alah_mt_prepositional |>
  filter(is.na(preposition_1) | preposition_1 == "") |>
  select(
    text_unit,
    chapter,
    verse_num,
    lex,
    cmpl_syntax,
    preposition_1
  )

#counting the prepositions

# column names
names(alah_mt)

alah_preposition_counts <- alah_mt_prepositional |>
  count(preposition_1, sort = TRUE) |>
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

alah_preposition_counts

# distribution of 'al with 'alah in MT books
alah_al_by_book <- alah_mt_prepositional |>
  group_by(text_unit) |>
  summarise(
    n_prepositional = n(),
    n_al = sum(preposition_1 == "<L", na.rm = TRUE),
    n_other = sum(preposition_1 != "<L", na.rm = TRUE),
    pct_al = round(n_al / n_prepositional * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(pct_al))

View(alah_al_by_book)

# focus on books with 10+ goals
alah_al_books_10plus <- alah_al_by_book |>
  filter(n_prepositional >= 10) |>
  arrange(desc(pct_al))

View(alah_al_books_10plus)


# check the use of 	MZBX/ with <LH[ in MT

alah_mt_mzbx <- alah_mt %>%
  filter(str_detect(cmpl_lex, fixed("MZBX/")))

View(alah_mt_mzbx)

dim(alah_mt_mzbx)

# footnote list
alah_mt_mzbx %>%
  transmute(reference = paste0(text_unit, " ", chapter, ":", verse_num)) %>%
  pull(reference) %>%
  paste(collapse = "; ")

# 'alah with mizbeah and 'al

alah_mt_mzbx %>%
  filter(cmpl_syntax == "prepositional") %>%
  summarise(
    total_prepositional = n(),
    al_count = sum(preposition_1 == "<L", na.rm = TRUE),
    al_percent = 100 * al_count / total_prepositional
  )