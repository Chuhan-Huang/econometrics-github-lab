# ---  Load and Prepare Data ---
install.packages("haven")
library(haven)
mydata <- read_dta("data/maketable5.dta")


# Create the base sample
install.packages("dplyr")
install.packages("tidyverse")
library(dplyr)
library(tidyverse)
base_sample <- mydata %>% filter(baseco == 1)

# Create the British colonies subsample
brit_colonies_sample <- base_sample %>% filter(f_brit == 1)

# ---  Run IV and OLS Regressions ---
# Run IV Models (for Panels A and B)
install.packages("fixest")
library(fixest)

iv_models <- list(
  "(1)" = feols(logpgp95 ~ f_brit + f_french | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst + f_brit + f_french | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = brit_colonies_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = brit_colonies_sample),
  "(5)" = feols(logpgp95 ~ sjlofr | avexpr ~ logem4, data = base_sample),
  "(6)" = feols(logpgp95 ~ lat_abst + sjlofr | avexpr ~ logem4, data = base_sample),
  "(7)" = feols(logpgp95 ~ catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample),
  "(9)" = feols(logpgp95 ~ f_french + sjlofr + catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample),
  "(10)" = feols(logpgp95 ~ lat_abst + f_french + sjlofr + catho80 + muslim80 + no_cpm80 | avexpr ~ logem4, data = base_sample)
)

# Run OLS Models (for Panel C)
ols_models <- list(
  "(1)" = feols(logpgp95 ~ avexpr + f_brit + f_french, data = base_sample),
  "(2)" = feols(logpgp95 ~ avexpr + lat_abst + f_brit + f_french, data = base_sample),
  "(3)" = feols(logpgp95 ~ avexpr, data = brit_colonies_sample),
  "(4)" = feols(logpgp95 ~ avexpr + lat_abst, data = brit_colonies_sample),
  "(5)" = feols(logpgp95 ~ avexpr + sjlofr, data = base_sample),
  "(6)" = feols(logpgp95 ~ avexpr + lat_abst + sjlofr, data = base_sample),
  "(7)" = feols(logpgp95 ~ avexpr + catho80 + muslim80 + no_cpm80, data = base_sample),
  "(8)" = feols(logpgp95 ~ avexpr + lat_abst + catho80 + muslim80 + no_cpm80, data = base_sample),
  "(9)" = feols(logpgp95 ~ avexpr + lat_abst + f_french + sjlofr + catho80 + muslim80 + no_cpm80, data = base_sample)
)

# ---  Generate Tables for Each Panel ---

# Panel A: 2SLS with Additional Controls
install.packages("modelsummary")
library(modelsummary)

modelsummary(
  iv_models,
  output = "gt",
  title = "Table 5, Panel A: IV Regressions of Log GDP Per Capita with Additional Controls",
  coef_map = c("fit_avexpr" = "Average Expropriation Risk", "lat_abst" = "Latitude",
               "f_brit" = "British Colony Dummy", "f_french" = "French Colony Dummy",
               "sjlofr" = "French Legal Origin", "catho80" = "Catholic Religion Dummy",
               "muslim80" = "Muslim Religion Dummy", "no_cpm80" = "Other Religion Dummy"),
  gof_map = "nobs",
  stars = TRUE
)