> library(haven)
> maketable4 <- read_dta("~/Desktop/maketable4.dta")
> View(maketable4)
> # ---  Load and Prepare Data ---
  > ajr_dta <- read_dta("~/Desktop/maketable4.dta")
  > 
    > # Create the base sample and the `other_cont` dummy variable
    > base_sample <- ajr_dta %>%
      +     filter(baseco == 1) %>%
      +     mutate(other_cont = if_else(shortnam %in% c("AUS", "MLT", "NZL"), 1, 0))
    > 
      > 
      > # Create the required subsamples
      > no_neo_europes_sample <- base_sample %>% filter(rich4 != 1)
      > no_africa_sample <- base_sample %>% filter(africa != 1)
      > # ---  Run All IV Regressions ---
        > iv_models <- list(
          +     "(1)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = base_sample),
          +     "(2)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = base_sample),
          +     "(3)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = no_neo_europes_sample),
          +     "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = no_neo_europes_sample),
          +     "(5)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = no_africa_sample),
          +     "(6)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = no_africa_sample),
          +     "(7)" = feols(logpgp95 ~ africa + asia + other_cont | avexpr ~ logem4, data = base_sample),
          +     "(8)" = feols(logpgp95 ~ lat_abst + africa + asia + other_cont | avexpr ~ logem4, data = base_sample),
          +     "(9)" = feols(loghjypl ~ 1 | avexpr ~ logem4, data = base_sample)
          + )
install.packages("modelsummary")
library(modelsummary)
# --- Generate Table for Panel A (2SLS Results) ---
install.packages("gt")
modelsummary(
  iv_models,
  output = "gt",
  title = "Table 4, Panel A: Two-Stage Least Squares Estimates",
  coef_map = c("fit_avexpr" = "Average protection against expropriation risk 1985-1995",
               "lat_abst"   = "Latitude",
               "africa"     = "Africa dummy",
               "asia"       = "Asia dummy",
               "other_cont" = "'Other' continent dummy"),
  gof_map = "nobs",
  stars = TRUE,
  notes = "Notes: 2SLS estimates with standard errors in parentheses."
)
# ---  Generate Table for Panel B (First Stage Results) ---



first_stage_models <- purrr::map(iv_models, ~.$iv_first_stage$avexpr)
# --------------------------------

modelsummary(
  first_stage_models,
  output = "gt",
  title = "Table 4, Panel B: First Stage for Average Protection Against Expropriation Risk",
  coef_map = c("logem4"     = "Log European settler mortality",
               "lat_abst"   = "Latitude",
               "africa"     = "Africa dummy",
               "asia"       = "Asia dummy",
               "other_cont" = "'Other' continent dummy",
               "(Intercept)" = "Constant"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE,
  notes = "Notes: First-stage OLS regressions. Dependent variable is Expropriation Risk. Standard errors in parentheses."
)
# --- Run Regressions for Panel C ---
panel_C_models <- list(
  "(1)" = feols(logpgp95 ~ avexpr, data = base_sample),
  "(2)" = feols(logpgp95 ~ avexpr + lat_abst, data = base_sample),
  "(3)" = feols(logpgp95 ~ avexpr, data = no_neo_europes_sample),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst, data = no_neo_europes_sample),
  "(5)" = feols(logpgp95 ~ avexpr, data = no_africa_sample),
  "(6)" = feols(logpgp95 ~ avexpr + lat_abst, data = no_africa_sample),
  "(7)" = feols(logpgp95 ~ avexpr + africa + asia + other_cont, data = base_sample),
  "(8)" = feols(logpgp95 ~ avexpr + lat_abst + africa + asia + other_cont, data = base_sample),
  "(9)" = feols(loghjypl ~ avexpr, data = base_sample)
)

        