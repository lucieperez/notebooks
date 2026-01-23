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

# 3) Sanity check: do all columns exist?
missing_cols <- setdiff(ivs, names(df))
if (length(missing_cols) > 0) {
  stop("These variables are not in df: ", paste(missing_cols, collapse = ", "))
}

# 4) Ensure they are factors (keeps labels as-is)
df <- df %>%
  mutate(across(all_of(ivs), ~ as.factor(.x)))


# 5) Print a clean summary: type, #levels, levels, missing
factor_audit <- map_dfr(ivs, function(v) {
  x <- df[[v]]
  lvls <- levels(x)
  tibble(
    variable = v,
    class = paste(class(x), collapse = ", "),
    n_levels = length(lvls),
    levels = paste(lvls, collapse = " | "),
    n_missing = sum(is.na(x))
  )
})

print(factor_audit)

# 6) Print frequency tables for each variable (including NA)
for (v in ivs) {
  cat("\n==============================\n")
  cat("Variable:", v, "\n")
  cat("==============================\n")
  print(addmargins(table(df[[v]], useNA = "ifany")))
}

# 7) show proportion tables
for (v in ivs) {
  cat("\n------------------------------\n")
  cat("Proportions:", v, "\n")
  cat("------------------------------\n")
  print(round(prop.table(table(df[[v]], useNA = "ifany")), 3))
}


###################### Independence and Association tests ######################


# Cramér's V test (effect size function)

cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  n <- sum(tab)
  r <- nrow(tab); k <- ncol(tab)
  v <- sqrt(as.numeric(chi$statistic) / (n * (min(r, k) - 1)))
  unname(v)
}

# this take in a contingency table (tab)
# computes a Chi Square test to get the chi square stats
# converts the chi square into Cramér's V (association strength for cat x cat variables)
# Interpretation (approx.):
    # 0.00–0.10: tiny
    # 0.10–0.20: small
    # 0.20–0.30: moderate
    # 0.30: strong

# Pairwise test function

# create the pairs to check
pairs <- combn(ivs, 2, simplify = FALSE)

# assumption checking function

# What are we looking for with the assumptions?
# chi square is acceptable is the minimum expected count is >= 1
# and if less than 20% of the cells have expected count inferior to 5

check_chisq_assumptions <- function(df, v1, v2) {
  
  tab <- table(df[[v1]], df[[v2]])
  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))
  
  tibble(
    var1 = v1,
    var2 = v2,
    n_cells = length(chi$expected),
    expected_min = min(chi$expected),
    expected_prop_lt5 = mean(chi$expected < 5)
  )
}

# Create the assumptions table
assumptions <- purrr::map_dfr(
  pairs,
  \(p) check_chisq_assumptions(df, p[1], p[2])
) %>%
  mutate(
    verdict = if_else(
      expected_min < 1 | expected_prop_lt5 > 0.20,
      "violation",
      "ok"
    )
  )

# Print assumptions in interpretation-friendly order
assumptions %>%
  mutate(verdict = factor(verdict, levels = c("violation", "ok"))) %>%
  arrange(verdict, expected_min, desc(expected_prop_lt5)) %>%
  print(n = Inf)

# What are we looking for with the assumptions?
# chi square is acceptable is the minimum expected count is >= 1
# and if less than 20% of the cells have expected count inferior to 5

# According to the table, 3 problematic pairs for Chi Square:
  # verse_genre x era_style : >20% under 5 (0.25)
  # motion_type x verse_genre: >20% under 5 (0.25)
  # motion_type x era_style: expected < 1 + >20% under 5 (0.52 and 0.208)
    # for these, the chi square test is not possible (Fisher?)


# Spliting the pairs for further analysis:
# re adding assumptions properly
assumptions <- assumptions %>%
  mutate(
    verdict = if_else(
      expected_min < 1 | expected_prop_lt5 > 0.20,
      "violation",
      "ok"
    )
  )

# check assumptions count
assumptions %>% count(verdict)

# create pairs for tests
chisq_pairs  <- assumptions %>% filter(verdict == "ok")
fisher_pairs <- assumptions %>% filter(verdict == "violation")

#check pairs content
print(chisq_pairs)
print(fisher_pairs)

# Association test for unproblematic pairs (chisq_pairs)

# Function for Cramér's V for Shared effect size 

cramers_v <- function(tab) {
  ht <- suppressWarnings(chisq.test(tab, correct = FALSE))
  n <- sum(tab)
  r <- nrow(tab); k <- ncol(tab)
  sqrt(as.numeric(ht$statistic) / (n * (min(r, k) - 1)))
}

# Chi Square Function
run_chisq <- function(df, v1, v2) {
  tab <- table(df[[v1]], df[[v2]])
  ht  <- suppressWarnings(chisq.test(tab, correct = FALSE))
  
  tibble(
    var1 = v1,
    var2 = v2,
    test = "chisq",
    statistic = unname(ht$statistic),
    df = unname(ht$parameter),
    p_value = unname(ht$p.value),
    cramers_v = cramers_v(tab)
  )
}


# Chi square and Cramér's V test

chisq_results <- purrr::map_dfr(
  seq_len(nrow(chisq_pairs)),
  function(i) run_chisq(df, chisq_pairs$var1[i], chisq_pairs$var2[i])
)

# print results
print(chisq_results)

# Association test for problematic pairs

# Function for Fisher's exact test

run_fisher <- function(df, v1, v2, B = 20000) {
  tab <- table(df[[v1]], df[[v2]])
  
  simulated <- FALSE
  ht <- tryCatch(
    fisher.test(tab),
    error = function(e) {
      simulated <<- TRUE
      fisher.test(tab, simulate.p.value = TRUE, B = B)
    }
  )
  
  tibble(
    var1 = v1,
    var2 = v2,
    test = ifelse(simulated, "fisher_sim", "fisher_exact"),
    statistic = NA_real_,
    df = NA_real_,
    p_value = unname(ht$p.value),
    cramers_v = cramers_v(tab)
  )
}


# run the test on the pairs
print(fisher_pairs)

fisher_results <- purrr::map(
  seq_len(nrow(fisher_pairs)),
  \(i) run_fisher(df, fisher_pairs$var1[i], fisher_pairs$var2[i])
) %>%
  dplyr::bind_rows()

# Print results 
print(fisher_results)

# Both association results in one table
association_results <- dplyr::bind_rows(
  chisq_results,
  fisher_results
) %>%
  arrange(desc(cramers_v))

association_results <- association_results %>%
  mutate(p_adj_fdr = p.adjust(p_value, method = "BH"))

print(association_results)

# Export the results to a csv file 

association_results_export <- association_results %>%
  arrange(desc(cramers_v)) %>%
  mutate(
    test = case_when(
      test == "chisq" ~ "Chi-square test",
      test == "fisher_exact" ~ "Fisher exact test",
      test == "fisher_sim" ~ "Fisher test (simulated p-value)",
      TRUE ~ test
    )
  )

readr::write_csv(
  association_results_export,
  "statistical_tests_results/indep_var_association_tests.csv"
)