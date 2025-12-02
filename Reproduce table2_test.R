
#loading packages
library(haven)
library(dplyr)
library(fixest)
library(gt)
library(modelsummary)

# ---  Load Data ---
ajr_dta <- read_dta("C:/Users/admin/Desktop/maketable2.dta")


# ---  Create Data Subsets ---
base_sample <- ajr_dta %>% filter(baseco == 1)

# ---  Run All Regressions ---
# Note on results: The public data has minor differences from the paper's,
# so coefficients may not match perfectly.
model_list <- list(
  "(1)" = feols(logpgp95 ~ avexpr, data = ajr_dta, se = "hetero"),
  "(2)" = feols(logpgp95 ~ avexpr, data = base_sample, se = "hetero"),
  "(3)" = feols(logpgp95 ~ avexpr + lat_abst, data = ajr_dta, se = "hetero"),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst + africa + asia + other, data = ajr_dta, se = "hetero"),
  "(5)" = feols(logpgp95 ~ avexpr + lat_abst, data = base_sample, se = "hetero"),
  "(6)" = feols(logpgp95 ~ avexpr + lat_abst + africa + asia + other, data = base_sample, se = "hetero"),
  "(7)" = feols(loghjypl ~ avexpr, data = ajr_dta, se = "hetero"),
  "(8)" = feols(loghjypl ~ avexpr, data = base_sample, se = "hetero")
)

# ---  Define Table Components ---
gof_map <- list(
  list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)
)


coef_map <- c("avexpr"    = "Average Expropriation Risk",
              "lat_abst"  = "Distance from Equator",
              "africa"    = "Africa",
              "asia"      = "Asia",
              "other"     = "Other continents")


modelsummary(
  model_list,
  output = "gt",
  title = "Table 2: OLS Regressions",
  coef_map = coef_map,
  gof_map = gof_map,
  stars = c('*' = .1, '**' = .05, '***' = .01),
  add_rows = tribble(
    ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)", ~"(7)", ~"(8)",
    "Base Sample", "No", "Yes", "No", "No", "Yes", "Yes", "No", "Yes",
    "Continent Dummies", "No", "No", "No", "Yes", "No", "Yes", "No", "No"
  ),
  notes = "Notes: Robust standard errors are in parentheses."
) %>%
  # Use gt's tab_spanner to create the correct headers
  tab_spanner(
    label = "Dependent variable: Log GDP per capita, 1995",
    columns = 2:7 # Selects columns for models (1) through (6)
  ) %>%
  tab_spanner(
    label = "Dependent variable: Log output per worker, 1988",
    columns = 8:9 # Selects columns for models (7) and (8)
  )

