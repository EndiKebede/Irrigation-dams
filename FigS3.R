library(dplyr)
library(tidyr)
#https://code.earthengine.google.com/b79439ef2ae7dd5eb15a122d66720386
# ---- Table 1 already: (2+3) totals in ha ----
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# ---- Load ----
df <- read_csv("DATA.csv", show_col_types = FALSE)

# ---- Standardize column names (adjust if yours differ) ----
df <- df %>%
  rename(
    country   = ADM0_NAME,
    year      = year,
    CA        = CA,          # if CA is stored as 10/50 in dH
    method    = Method,      # change if your column is named differently
    cropClass = cropClass,
    ha        = ha
  ) %>%
  mutate(
    year = as.integer(year),
    CA = case_when(
      as.numeric(CA) == 10 ~ "CA10",
      as.numeric(CA) == 50 ~ "CA50",
      TRUE ~ paste0("CA", CA)
    ),
    method = as.factor(method),
    country = as.factor(country)
  )

# ---- (i) Irrigated area = cropClass 2 or 3; sum ha by country, year, CA, method ----
irr_ctrym <- df %>%
  filter(cropClass %in% c(2, 3)) %>%
  group_by(country, year, CA, method) %>%
  summarise(irr_ha = sum(ha, na.rm = TRUE), .groups = "drop")

# ---- (ii) Keep CA10; plot log10 irrigated ha vs year by country (color) and method (linetype) ----
irr_ca10 <- irr_ctrym %>%
  filter(CA == "CA10") %>%
  mutate(log10_irr_ha = log10(irr_ha))   # assumes irr_ha > 0

ggplot(irr_ca10, aes(
  x = year, y = log10_irr_ha,
  color = country, linetype = method,
  group = interaction(country, method)
)) +
  # your existing points (and keep your lines if you want)
  geom_point(alpha = 0.7, size = 1.6) +
  geom_line(alpha = 0.7, linewidth = 0.7) +
  
  # OVERLAID method-level trendlines (two lines total), in black
  geom_smooth(
    data = irr_ca10,
    aes(x = year, y = log10_irr_ha, linetype = method, group = method),
    inherit.aes = FALSE,
    method = "lm", se = FALSE,
    color = "black", linewidth = 1.4
  ) +
  labs(x = "Year", y = "log10 irrigated area (ha)",
       color = "Country", linetype = "Method") +
  theme_minimal()

# Packages
library(dplyr)
library(sandwich)
library(lmtest)
library(broom)
library(knitr)
library(kableExtra)

# --- your model code assumed already run, producing: dat, m, vc ---

# Tidy regression output with clustered SE
reg <- broom::tidy(m, vcov = vc) %>%
  mutate(
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 2),
    p.value = signif(p.value, 3)
  )

# Nice labels (optional but recommended)
term_labels <- c(
  "(Intercept)"      = "Intercept",
  "year"             = "Year",
  "methodNDVI"       = "NDVI (vs AEI)",
  "year:methodNDVI"  = "Year × NDVI (slope diff vs AEI)"
)

reg <- reg %>%
  mutate(term = ifelse(term %in% names(term_labels), term_labels[term], term))

# Build table
tab <- reg %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  rename(
    Term = term,
    Estimate = estimate,
    `Clustered SE` = std.error,
    `t` = statistic,
    `p` = p.value
  )

caption_txt <- paste0(
  "OLS regression of log10 irrigated area (ha) within CA10 on year, method, and their interaction, ",
  "including country fixed effects. Irrigated area is defined as the sum of hectares in cropClass 2 and 3. ",
  "The reference method is AEI, so the coefficient on 'Year × NDVI' is the NDVI–AEI difference in the annual trend. ",
  "Standard errors are clustered at the country level."
)

kable(tab, caption = caption_txt, align = "lrrrr") %>%
  kable_styling(full_width = FALSE)