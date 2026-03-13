# Packages
library(tidybayes)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggdist)
library(readr)
library(stringr)

# MODEL
motion_verb_1 <- readRDS("models/motion_verb_1.rds")

# check that the variables are present
head(get_variables(motion_verb_1), 50)

### FIXED EFFECTS / INDEPENDENT VARIABLES ###

# extract fixed effects (all b_* parameters like b_mudirhe_cmpl_animanim)

fixed_draws <- motion_verb_1 %>%
  gather_draws(`b_.*`, regex = TRUE) %>%   # returns .variable and .value
  mutate(param = sub("^b_", "", .variable)) %>%
  # 2) Split into outcome-category (dpar) + term
  # For categorical models, param often starts with mu<category>_...
  # If your model names do not start with mu, this will still work (dpar will just be the first chunk).
  separate(param, into = c("dpar", "term"), sep = "_", extra = "merge", fill = "right") %>%
  # Clean up common patterns
  mutate(
    dpar = sub("^mu", "", dpar),  # turns "muother" into "other" (if applicable)
    term = ifelse(is.na(term) | term == "", dpar, term)  # safety fallback
  )

#  Reorder terms within each facet so the plot is readable
fixed_draws <- fixed_draws %>%
  group_by(dpar) %>%
  mutate(term = fct_reorder(term, .value, .fun = median)) %>%
  ungroup()

# Change the names of fixed effects for easier reading 

fixed_draws <- fixed_draws %>%
  mutate(
    term = term %>%
      # verse_genreX -> verse_genre: X
      gsub("^verse_genre", "verse_genre: ", .) %>%
      # era_styleX -> era_style: X
      gsub("^era_style", "era_style: ", .) %>%
      # motion_typeX -> motion_type: X
      gsub("^motion_type", "motion_type: ", .) %>%
      # cmpl_*X -> cmpl_*: X
      gsub("^cmpl_anim", "cmpl_anim: ", .) %>%
      gsub("^cmpl_det", "cmpl_det: ", .) %>%
      gsub("^cmpl_complex", "cmpl_complex: ", .)
  )

fixed_draws <- fixed_draws %>%
  mutate(
    dpar = recode(
      dpar,
      dirhe = "directive -he",
      vc    = "unmarked complement"
    )
  )

# Chose the order of the rows for easier interpretation:

term_order <- c(
  "Intercept",
  # syntactico-semantic
  "cmpl_anim:",
  "cmpl_complex:",
  "cmpl_det:",
  "motion_type:",
  # diachronic
  "era_style:",
  # discourse / genre
  "verse_genre:"
)

fixed_draws <- fixed_draws %>%
  mutate(
    term = factor(
      term,
      levels = {
        intercept <- "Intercept"
        
        syntactic <- term[grepl("^cmpl_anim:|^cmpl_complex:|^cmpl_det:|^motion_type:", term)]
        era       <- term[grepl("^era_style:", term)]
        genre     <- term[grepl("^verse_genre:", term)]
        
        c(
          intercept,
          sort(unique(syntactic)),
          sort(unique(era)),
          sort(unique(genre))
        )
      }
    )
  )

# Plot: Faceted halfeye of fixed effects by outcome category
ggplot(fixed_draws, aes(x = .value, y = term)) +
  geom_vline(xintercept = 0, linetype = 2) +
  stat_halfeye(.width = c(.5, .8, .95)) +
  facet_wrap(~ dpar, scales = "free_y") +
  scale_y_discrete(limits = rev(levels(fixed_draws$term))) +
  labs(
    x = "Coefficient (log-odds scale)",
    y = NULL,
    title = "Fixed effects by non-reference outcome category"
  )

# to save the plot, add p <- before plotting, then:
# ggsave("plots/fixed_effects_coefficients.png", p, width = 12, height = 8, dpi = 300)



### RANDOM EFFECTS / GROUP LEVEL VARIABLES ###

# Fixed intercepts by outcome category (dpar) 
b_int <- motion_verb_1 %>%
  gather_draws(`b_.*Intercept`, regex = TRUE) %>%
  ungroup() %>%
  transmute(
    .draw,
    param = sub("^b_", "", .variable),
    b_Intercept = .value
  ) %>%
  separate(param, into = c("dpar_raw", "rest"), sep = "_", extra = "merge", fill = "right") %>%
  mutate(dpar = sub("^mu", "", dpar_raw)) %>%
  select(.draw, dpar, b_Intercept)

# Random intercepts (book_scroll) 
r_book <- motion_verb_1 %>%
  gather_draws(`r_book_scroll.*`, regex = TRUE) %>%
  ungroup() %>%
  filter(str_detect(.variable, ",Intercept\\]$")) %>%
  transmute(
    .draw,
    dpar = str_match(.variable, "^r_book_scroll__([^\\[]+)\\[")[,2],
    level = str_match(.variable, "\\[([^,]+),Intercept\\]")[,2],
    r = .value
  ) %>%
  mutate(
    dpar = sub("^mu", "", dpar),
    group = "book_scroll"
  )

# Random intercepts (lex)
r_lex <- motion_verb_1 %>%
  gather_draws(`r_lex.*`, regex = TRUE) %>%
  ungroup() %>%
  filter(str_detect(.variable, ",Intercept\\]$")) %>%
  transmute(
    .draw,
    dpar = str_match(.variable, "^r_lex__([^\\[]+)\\[")[,2],
    level = str_match(.variable, "\\[([^,]+),Intercept\\]")[,2],
    r = .value
  ) %>%
  mutate(
    dpar = sub("^mu", "", dpar),
    group = "lex"
  )

# Conditional intercepts = fixed intercept + random intercept
cond_all <- bind_rows(r_book, r_lex) %>%
  left_join(b_int, by = c(".draw", "dpar")) %>%
  mutate(cond_intercept = b_Intercept + r) %>%
  select(.draw, dpar, group, level, cond_intercept)

names(cond_all)

# LEXEMES

lex_order <- cond_all %>%
  filter(group == "lex") %>%
  group_by(dpar, level) %>%
  summarise(med = median(cond_intercept), .groups = "drop") %>%
  arrange(dpar, med)

# assign to different plot pages for better readability
lex_pages <- lex_order %>%
  group_by(dpar) %>%
  mutate(page = ceiling(row_number() / 15)) %>%
  ungroup()

# draws info

lex_draws <- cond_all %>%
  filter(group == "lex") %>%
  left_join(
    lex_pages %>% select(dpar, level, page),
    by = c("dpar", "level")
  )

# plot page 1

ggplot(
  lex_draws %>% filter(page == 1),
  aes(y = level, x = cond_intercept)
) +
  geom_vline(xintercept = 0, linetype = 2) +
  stat_halfeye(.width = c(.5, .8, .95)) +
  facet_wrap(~ dpar, scales = "free_y") +
  scale_y_discrete(limits = rev) +
  labs(
    x = "Conditional intercept (log-odds vs reference `prep`)",
    y = NULL,
    title = "Lexeme-level baseline tendencies (page 1)"
  )
    
# plot page 2

ggplot(
  lex_draws %>% filter(page == 2),
  aes(y = level, x = cond_intercept)
) +
  geom_vline(xintercept = 0, linetype = 2) +
  stat_halfeye(.width = c(.5, .8, .95)) +
  facet_wrap(~ dpar, scales = "free_y") +
  scale_y_discrete(limits = rev) +
  labs(
    x = "Conditional intercept (log-odds vs reference `prep`)",
    y = NULL,
    title = "Lexeme-level baseline tendencies (page 2)"
  )

# plot page 3

ggplot(
  lex_draws %>% filter(page == 3),
  aes(y = level, x = cond_intercept)
) +
  geom_vline(xintercept = 0, linetype = 2) +
  stat_halfeye(.width = c(.5, .8, .95)) +
  facet_wrap(~ dpar, scales = "free_y") +
  scale_y_discrete(limits = rev) +
  labs(
    x = "Conditional intercept (log-odds vs reference `prep`)",
    y = NULL,
    title = "Lexeme-level baseline tendencies (page 3)"
  )

## BOOK_SCROLLS

per_page_book <- 16

book_levels <- cond_all %>%
  filter(group == "book_scroll") %>%
  distinct(level) %>%
  arrange(level) %>%                       # or arrange by something else (see note below)
  mutate(page = ceiling(row_number() / per_page_book))

# 2) Attach page to ALL draws (all dpars) for those scrolls
book_draws <- cond_all %>%
  filter(group == "book_scroll") %>%
  left_join(book_levels, by = "level")

# 3) Nice outcome labels
book_draws_plot <- book_draws %>%
  mutate(
    dpar = recode(dpar,
                  dirhe = "directive -he",
                  vc    = "unmarked complement")
  )

# Plot one page (rows now line up across facets)
ggplot(book_draws_plot %>% filter(page == 1),
       aes(y = level, x = cond_intercept)) +
  geom_vline(xintercept = 0, linetype = 2) +
  stat_halfeye(.width = c(.5, .8, .95)) +
  facet_wrap(~ dpar, ncol = 2) +           # IMPORTANT: shared y-axis (no free_y)
  scale_y_discrete(limits = rev) +
  labs(
    x = "Conditional intercept (log-odds vs reference `prep`)",
    y = NULL,
    title = "Book/Scroll baseline tendencies (page 1)"
  )

# Save all pages to one PDF
n_pages <- max(book_draws_plot$page, na.rm = TRUE)

pdf("plots/book_scroll_halfeye_pages.pdf", width = 10, height = 6)
for (p in 1:n_pages) {
  print(
    ggplot(book_draws_plot %>% filter(page == p),
           aes(y = level, x = cond_intercept)) +
      geom_vline(xintercept = 0, linetype = 2) +
      stat_halfeye(.width = c(.5, .8, .95)) +
      facet_wrap(~ dpar, ncol = 2) +
      scale_y_discrete(limits = rev) +
      labs(
        x = "Conditional intercept (log-odds vs reference `prep`)",
        y = NULL,
        title = paste0("Book/Scroll baseline tendencies (page ", p, " / ", n_pages, ")")
      )
  )
}
dev.off()

### Generate a specific file with only MT and the three sectarian Qumran scrolls 1QS, 1QM and 1QH.

per_page_book <- 6

keep_qumran <- c("1QS_1QS", "1QM_1QM", "1QH_1QH")

# Canonical order
tanakh_order <- c(
  # Torah
  "Genesis","Exodus","Leviticus","Numbers","Deuteronomy",
  # Nevi'im
  "Joshua","Judges","Samuel","Kings","Isaiah","Jeremiah","Ezekiel",
  "Hosea","Joel","Amos","Obadiah","Jonah","Micah","Nahum","Habakkuk","Zephaniah","Haggai","Zechariah","Malachi",
  # Ketuvim
  "Psalms","Proverbs","Job","Song_of_songs","Ruth","Lamentations","Ecclesiastes","Esther",
  "Daniel","Ezra","Nehemiah","Chronicles"
)

# Helper: extract book name from your level strings like "Genesis_MT"
# (removes trailing _MT and then takes text before the first underscore)
extract_book <- function(x) {
  x %>%
    str_remove("_MT$") %>%
    str_extract("^[^_]+")
}

book_levels_filt <- cond_all %>%
  filter(group == "book_scroll") %>%
  distinct(level) %>%
  filter(str_detect(level, "_MT$") | level %in% keep_qumran) %>%
  mutate(
    is_qumran = level %in% keep_qumran,
    book = extract_book(level),
    # Map book to canonical position; unknown books get pushed after known Tanakh books
    book_ord = match(book, tanakh_order),
    book_ord = if_else(is.na(book_ord), 999L, as.integer(book_ord))
  ) %>%
  arrange(is_qumran, book_ord, book, level) %>%   # Qumran (TRUE) will come last
  mutate(page = ceiling(row_number() / per_page_book)) %>%
  select(level, page)

# Attach page to ALL draws (all dpars) for the selected scrolls
book_level_order <- book_levels_filt$level  # preserves your canonical + Qumran-last order

book_draws_filt <- cond_all %>%
  filter(group == "book_scroll") %>%
  semi_join(book_levels_filt, by = "level") %>%
  left_join(book_levels_filt, by = "level") %>%
  mutate(
    level = factor(level, levels = book_level_order),  # IMPORTANT: fixes alphabetical y-order
    dpar = recode(dpar,
                  dirhe = "directive -he",
                  vc    = "unmarked complement")
  )

# Save to PDF
dir.create("plots", showWarnings = FALSE, recursive = TRUE)  # IMPORTANT: ensures folder exists
n_pages <- max(book_draws_filt$page, na.rm = TRUE)

pdf("plots/book_scroll_halfeye_pages_MT__plus_sect_qumran.pdf",
    width = 10, height = 7)

for (p in 1:n_pages) {
  
  plot_data <- book_draws_filt %>%
    filter(page == p) %>%
    mutate(level = forcats::fct_rev(forcats::fct_drop(level)))
  
  g <- ggplot(plot_data,
              aes(y = level, x = cond_intercept)) +
    geom_vline(xintercept = 0, linetype = 2) +
    stat_halfeye(.width = c(.5, .8, .95)) +
    facet_wrap(~ dpar, ncol = 2) +
    scale_y_discrete(drop = TRUE) +
    labs(
      x = "Conditional intercept (log-odds vs reference `prep`)",
      y = NULL,
      title = paste0(
        "MT books (canonical order) + sectarian Qumran — page ",
        p, " / ", n_pages
      )
    )
  
  print(g)
}

dev.off()


### CREATE CSV FILES

summ_ci <- function(df) {
  df %>%
    group_by(dpar, level) %>%
    summarise(
      mean   = mean(cond_intercept),
      median = median(cond_intercept),
      lo_95  = quantile(cond_intercept, 0.025),
      lo_80  = quantile(cond_intercept, 0.10),
      lo_50  = quantile(cond_intercept, 0.25),
      hi_50  = quantile(cond_intercept, 0.75),
      hi_80  = quantile(cond_intercept, 0.90),
      hi_95  = quantile(cond_intercept, 0.975),
      .groups = "drop"
    )
}

# Lexeme CSV
lex_csv <- cond_all %>%
  filter(group == "lex") %>%
  summ_ci()

view(lex_csv)

write_csv(lex_csv, "plots/lexeme_conditional_intercepts.csv")

# Book/scroll CSV
book_csv <- cond_all %>%
  filter(group == "book_scroll") %>%
  summ_ci()

view(book_csv)

write_csv(book_csv, "plots/book_scroll_conditional_intercepts.csv")

### TEST LEXEME CODE

per_page_lex <- 15

# 1) Page assignment ONLY by lexeme level (alphabetical; change arrange() if desired)
lex_levels <- cond_all %>%
  filter(group == "lex") %>%
  distinct(level) %>%
  arrange(level) %>%
  mutate(page = ceiling(row_number() / per_page_lex))

# 2) Attach page to ALL draws (all dpars) for those lexemes
lex_draws <- cond_all %>%
  filter(group == "lex") %>%
  left_join(lex_levels, by = "level") %>%
  mutate(
    dpar = recode(dpar,
                  dirhe = "directive -he",
                  vc    = "unmarked complement")
  )

# Helper: plot one page with aligned rows
plot_lex_page <- function(p) {
  plot_data <- lex_draws %>%
    filter(page == p) %>%
    mutate(level = fct_drop(level))  # drop unused levels so only 15 rows show
  
  ggplot(plot_data, aes(y = level, x = cond_intercept)) +
    geom_vline(xintercept = 0, linetype = 2) +
    stat_halfeye(.width = c(.5, .8, .95)) +
    facet_wrap(~ dpar, ncol = 2) +        # shared y-axis: rows line up
    scale_y_discrete(drop = TRUE, limits = rev) +
    labs(
      x = "Conditional intercept (log-odds vs reference `prep`)",
      y = NULL,
      title = paste0("Lexeme-level baseline tendencies (page ", p, ")")
    )
}

# 3) Your three plots
plot_lex_page(1)
plot_lex_page(2)
plot_lex_page(3)

# save all pages to one PDF
dir.create("plots", showWarnings = FALSE, recursive = TRUE)
n_pages <- max(lex_draws$page, na.rm = TRUE)

pdf("plots/lexeme_halfeye_pages.pdf", width = 10, height = 6)
for (p in 1:n_pages) print(plot_lex_page(p))
dev.off()
