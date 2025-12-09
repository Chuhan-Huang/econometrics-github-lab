library(ggplot2)
library(ggrepel) 
library(dplyr)
library(fixest)
library(modelsummary)
library(gt)

# ---  Load and Prepare Data ---
ajr_dta <- read_dta("F:/xwechat_files/wxid_vsogc03zvbuq12_4ee5/msg/file/2025-12/maketable4.dta")

# Create the base sample and the `other_cont` dummy variable
base_sample <- ajr_dta %>%
  filter(baseco == 1) %>%
  mutate(other_cont = if_else(shortnam %in% c("AUS", "MLT", "NZL"), 1, 0))

# Create the required subsamples
no_neo_europes_sample <- base_sample %>% filter(rich4 != 1)
no_africa_sample <- base_sample %>% filter(africa != 1)

# ---  Run All IV Regressions ---
iv_models <- list(
  "(1)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = base_sample),
  "(2)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = base_sample),
  "(3)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = no_neo_europes_sample),
  "(4)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = no_neo_europes_sample),
  "(5)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = no_africa_sample),
  "(6)" = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = no_africa_sample),
  "(7)" = feols(logpgp95 ~ africa + asia + other_cont | avexpr ~ logem4, data = base_sample),
  "(8)" = feols(logpgp95 ~ lat_abst + africa + asia + other_cont | avexpr ~ logem4, data = base_sample),
  "(9)" = feols(loghjypl ~ 1 | avexpr ~ logem4, data = base_sample)
)

# --- Generate Table for Panel A (2SLS Results) ---
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
## NOTE: 3 observations removed because of NA values (LHS: 3).

# --- Generate Table for Panel C ---
modelsummary(
  panel_C_models,
  output = "gt",
  title = "Table 4, Panel C: OLS Regressions",
  coef_map = c("avexpr" = "Protection Against Expropriation Risk", "lat_abst" = "Distance from Equator",
               "africa" = "Africa", "asia" = "Asia", "other_cont" = "Other Continents"),
  gof_map = list(list("raw" = "nobs", "clean" = "Num. Obs.", "fmt" = 0),
                 list("raw" = "r.squared", "clean" = "R-squared", "fmt" = 3)),
  stars = TRUE,
  notes = "Notes: OLS regressions with standard errors in parentheses."
)

# --- Extension: Robustness Check (Capping Mortality at 250) ---
base_sample_ext <- base_sample %>%
  mutate(
    mortality_raw = exp(logem4), 
    mortality_capped_val = if_else(mortality_raw > 250, 250, mortality_raw),
    logem4_capped = log(mortality_capped_val)
  )


# 2. Fixing the crowded plot
p_clean <- ggplot(base_sample_ext, aes(x = logem4, y = logem4_capped)) +
  geom_point(alpha = 0.7, color = "#2c3e50") + 
  geom_text_repel(
    aes(label = if_else(logem4 > 6.0, shortnam, "")), 
    box.padding = 0.5,
    min.segment.length = 0, 
    color = "#e74c3c",      
    fontface = "bold"
  ) +
  labs(
    title = "Extension: Capping Extreme Mortality Rates at 250",
    subtitle = "Labels shown only for extreme outliers (Original Log Mortality > 6.0)",
    x = "Original Log Settler Mortality (AJR 2001)",
    y = "Capped Log Settler Mortality (AJR 2012 Strategy)"
  ) +
  theme_minimal()

print(p_clean)

# 3. Re-run Key Regressions with the Capped IV
iv_extension_simple <- list(
  # Baseline Model (No controls)
  "Original (Base)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = base_sample_ext),
  "Capped (Base)"   = feols(logpgp95 ~ 1 | avexpr ~ logem4_capped, data = base_sample_ext),
  
  # Model controlling for Latitude
  "Original (Lat)"  = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = base_sample_ext),
  "Capped (Lat)"    = feols(logpgp95 ~ lat_abst | avexpr ~ logem4_capped, data = base_sample_ext)
)

# Generate figure
modelsummary(
  iv_extension_simple,
  output = "gt",
  title = "Robustness Check: Effect of Capping Mortality (Base Sample)",
  coef_map = c("fit_avexpr" = "Institutions (Expropriation Risk)",
               "lat_abst"   = "Latitude"),
  gof_map = list(list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
                 list("raw" = "iv_fstat_first_stage", "clean" = "1st Stage F", "fmt" = 2)),
  stars = TRUE,
  notes = "Note: 'Capped' models restrict mortality rates to a maximum of 250 before logging."
)