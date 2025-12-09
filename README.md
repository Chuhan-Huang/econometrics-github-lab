# Replication Files for Acemoglu, Johnson, and Robinson (2001)

================

This GitHub repository contains the data and R code used to replicate
selected tables and empirical results from:

Acemoglu, Johnson, and Robinson (2001)
“The Colonial Origins of Comparative Development: An Empirical Investigation”
American Economic Review

The goal of this project is to reproduce the main regression tables of the paper.
Our group of three members each replicated different sections (Table 2, Table 4, and Table 5) of the original paper using publicly available data.

# Folder Structure

The recommended folder structure for this replication project is:

Folder	Usage
code	R scripts for all replications
data	Original Stata .dta files used in the analysis
tables	Output regression tables
output	Additional estimation results
graphs	(Optional) Figures and visualizations

# Data Files

All datasets are loaded from Stata .dta files corresponding to the original AJR paper:

Data File	Description	Used For
<br>maketable2.dta	Data used to replicate Table 2 (OLS regressions)	Table 2
  maketable4.dta	Data used to replicate Table 4 (2SLS and first stage)	Table 4
  maketable5.dta	Data used to replicate Table 5 (IV with additional controls)	Table 5

# Replicated Tables and Responsibilities

Each group member replicated one main table from the original paper:

Table	Description	Method
Table 2	OLS relationship between institutions and income	OLS
Table 4	IV regressions using settler mortality + first stage	2SLS
Table 5	IV regressions with additional institutional, legal, and religion controls	2SLS + OLS

# R Scripts and Workflow

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
# Note on results: The public data has minor differences from the paper's, so coefficients may not match perfectly.
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

        




# ---  Load and Prepare Data ---
install.packages("haven")
library(haven)
mydata <- read_dta("/Users/jingxiaorui/Desktop/data/maketable5.dta")


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




# Robustness Extension (AJR 2012)

Kaiyi Deng

### **Overview**

Follwoing up replicating Table 4 from Acemoglu, Johnson, and Robinson (2001), I include an extension based on AJR (2012), implementing a "mortality capping" strategy to test the robustness of the results against extreme outliers (e.g., Mali, Gambia).

**Dataset:** maketable4.dta - The original dataset provided by AJR (2001).

### **Motivation**

Critics of the original paper, such as Albouy (2012), argued that the results were driven by specific outliers with extremely high mortality rates, particularly in Africa (e.g., Mali and Gambia). These rates often reflected epidemics rather than the long-term mortality risks considered by settlers.

In their reply, AJR (2012) proposed a "capping" strategy. They argued that mortality rates above a certain threshold (e.g., 250 deaths per 1000) would have the same deterrent effect on settlement as higher rates. Therefore, capping the data reduces the influence of measurement error from epidemics while preserving the variation useful for identification.

### **Methodology & Code**

I implemented this robustness check by creating a new variable, logem4_capped. I capped the raw mortality rate at 250 per 1000 before taking the logarithm, as suggested by AJR (2012).

**Prerequisites:** Installed the following packages before running

```{r}
library(ggplot2)
library(ggrepel) 
library(dplyr)
library(fixest)
library(modelsummary)
library(gt)
```

**Extension: Robustness Check (Capping Mortality at 250)**

```{r}
#Revert the log mortality to raw levels, apply the cap at 250,and re-logarithmize it.

base_sample_ext <- base_sample %>%
  mutate(
    # Convert log mortality back to raw mortality level
    mortality_raw = exp(logem4),
    
    # Apply the cap: If mortality > 250, set it to 250
    mortality_capped_val = if_else(mortality_raw > 250, 250, mortality_raw),
    
    # Take the log of the capped value
    logem4_capped = log(mortality_capped_val)
  )
```

**Visualization of Outliers**

I plotted the original mortality against the capped mortality. I specifically labeled extreme outliers (Original Log Mortality \> 6.0) to highlight the countries driving the debate, such as Mali and Gambia.

```{r}
p_clean <- ggplot(base_sample_ext, aes(x = logem4, y = logem4_capped)) +
  geom_point(alpha = 0.7, color = "#2c3e50") + 
 #Mark only extreme values
  geom_text_repel(
    aes(label = if_else(logem4 > 6.0, shortnam, "")), 
    box.padding = 0.5,
    min.segment.length = 0, # Always show connections  
    color = "#e74c3c",  # Red labels highlighted    
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
```

![](images/clipboard-2274002174.png)

**Regression Results & Interpretation**

I re-ran the core 2SLS models using the new capped instrument (logem4_capped).

```{r}
iv_extension_simple <- list(
  # Baseline Model (No controls)
  "Original (Base)" = feols(logpgp95 ~ 1 | avexpr ~ logem4, data = base_sample_ext),
  "Capped (Base)"   = feols(logpgp95 ~ 1 | avexpr ~ logem4_capped, data = base_sample_ext),
  
  # Model controlling for Latitude
  "Original (Lat)"  = feols(logpgp95 ~ lat_abst | avexpr ~ logem4, data = base_sample_ext),
  "Capped (Lat)"    = feols(logpgp95 ~ lat_abst | avexpr ~ logem4_capped, data = base_sample_ext)
)
```

**Generate table:**

```{r}
modelsummary(
  iv_extension_simple, # The list of regression model objects to summarize (Original vs. Capped)
  output = "gt", # Use the 'gt' package to generate a beautifully styled table 
  title = "Robustness Check: Effect of Capping Mortality (Base Sample)",
  coef_map = c("fit_avexpr" = "Institutions (Expropriation Risk)",
               "lat_abst"   = "Latitude"),
  gof_map = list(list("raw" = "nobs", "clean" = "Obs.", "fmt" = 0),
                 list("raw" = "iv_fstat_first_stage", "clean" = "1st Stage F", "fmt" = 2)),
  stars = TRUE,
  notes = "Note: 'Capped' models restrict mortality rates to a maximum of 250 before logging."
)
```

![](images/clipboard-2373645936.png)

**Econometric Interpretation:** By comparing the "Original" and "Capped" columns in our output table, I find that the coefficient on institutions remains positive and statistically significant even after capping the extreme mortality values. This supports the argument in AJR (2012) that the main results are robust and are not artifacts of a few African outliers with epidemic-level mortality rates.

