# Packages
library(tidyverse)
library(janitor)
library(rcompanion)

# 1) Load data
df <- readr::read_csv("data/filtered_dataset.csv", show_col_types = FALSE) %>%
  clean_names()

# 2) Independent variables (IVs) to check
ivs <- c("cmpl_anim","cmpl_det","cmpl_complex","motion_type","verse_genre","era_style")

# 3) All unique pairs
pairs <- combn(ivs, 2, simplify = FALSE)

# 4) Is chi-square reliable (chi-square assumptions)
chisq_ok <- function(expected) {
  min(expected) >= 1 && mean(expected < 5) <= 0.20
}

# 5) Run one pair (returns one tidy row)

# the following function takes one pair of independent variables and:
# - builds the contingency table
# - computes the expected counts (for chi square assumptions)
# - decides of the appropriate test (Chi square or fisher)
# - runs the chosen test
# - computes Cramér's V (strength of the association)
# - returns a row with all the tests results and Cramér's V


analyze_pair <- function(df, v1, v2, B = 20000) {
  
  # observed counts
  tab <- table(df[[v1]], df[[v2]])
  
  # expected counts using chi square (for diagnostics + test choice)
  chi_obj <- suppressWarnings(chisq.test(tab, correct = FALSE))
  expected <- chi_obj$expected
  
  # select the appropriate test based on expected counts
  if (chisq_ok(expected)) {
    test_name <- "chisq"
    p_value   <- unname(chi_obj$p.value)
    statistic <- unname(chi_obj$statistic)
    dfree     <- unname(chi_obj$parameter)
  } else {
    # runs Fisher's Exact test if the chi square assumptions are violated:
    fisher_obj <- tryCatch(
      fisher.test(tab),
      error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = B)
    )
    test_name <- "fisher"
    p_value   <- unname(fisher_obj$p.value)
    statistic <- NA_real_ # puts NA in the statistic and dfree columns 
    dfree     <- NA_real_ # as Fisher does not have these elements
  }
  
  # create the row with test results and adds Cramér's V
  tibble(
    var1 = v1,
    var2 = v2,
    n_cells = length(expected),
    expected_min = min(expected),
    expected_prop_lt5 = mean(expected < 5),
    test = test_name,
    statistic = statistic,
    df = dfree,
    p_value = p_value,
    cramers_v = rcompanion::cramerV(tab) # uses the contingency table tab to compute Cramér's V
  )
}

# 6) Run all pairs
association_results <- purrr::map_dfr(
  pairs,
  \(p) analyze_pair(df, p[1], p[2])
) %>%
  mutate(p_adj_fdr = p.adjust(p_value, method = "BH")) %>%
  arrange(desc(cramers_v))

# 7) print the results
print(association_results)

# 8) save to a csv file

readr::write_csv(
  association_results,
  "statistical_tests_results/predictor_associations_chisq_fisher_cramersV.csv"
)

view(association_results)