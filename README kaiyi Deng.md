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
