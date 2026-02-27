# ============================================================
# RQ1: Trait variability across MAT–MAP climate space (GAMM)
# Traits: LMA + Nmass
# Response: log10(std_value)
# Fixed effects: smooth(MAT) + smooth(MAP)
# Random effects: grid_id (lat/lon-based) + species
# ============================================================

library(tidyverse)
library(janitor)
library(mgcv)
library(scales)

# --------------------------
# 1) Load + clean
# --------------------------
df <- read_csv("traits_with_MAT_MAP_ERA5Land_1991_2020_withLonLat.csv") %>%
  clean_names()

df1 <- df %>%
  filter(!is.na(trait_name), !is.na(std_value), !is.na(mat_c), !is.na(map_mm),
         !is.na(latitude), !is.na(longitude), !is.na(acc_species_name)) %>%
  filter(std_value > 0) %>%  # needed for log10
  mutate(
    trait_name = as.factor(trait_name),
    trait_log10 = log10(std_value),
    
    # "grid cell" id from rounded lat/lon (acts as location random effect)
    grid_id = paste0(round(latitude, 2), "_", round(longitude, 2)),
    grid_id = as.factor(grid_id),
    
    # species random effect
    acc_species_name = as.factor(acc_species_name)
  )



# --------------------------
# 2) Plot theme (optional)
# --------------------------
theme_clean_panel <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
}



# --------------------------
# 3) Fit GAMM for one trait
# --------------------------
fit_gamm_trait <- function(trait_nm, k_mat = 10, k_map = 10) {
  
  dat <- df1 %>% filter(trait_name == trait_nm)
  
  m <- gam(
    trait_log10 ~
      s(mat_c, k = k_mat) +
      s(map_mm, k = k_map) +
      s(grid_id, bs = "re") +
      s(acc_species_name, bs = "re"),
    data = dat,
    method = "REML"
  )
  
  list(data = dat, model = m)
}



# --------------------------
# 4) Run models (LMA + Nmass)
# --------------------------
trait_lma   <- "Leaf Mass per Area"
trait_nmass <- "Leaf nitrogen (N) content per leaf dry mass"


fit_lma   <- fit_gamm_trait(trait_lma)
fit_nmass <- fit_gamm_trait(trait_nmass)


m_lma   <- fit_lma$model
m_nmass <- fit_nmass$model



# --------------------------
# 5) Summaries + diagnostics
# --------------------------
summary(m_lma)
gam.check(m_lma)

summary(m_nmass)
gam.check(m_nmass)

# --------------------------
# 6) Smooth plots (MAT and MAP effects)
# --------------------------
par(mfrow = c(1, 2))
plot(m_lma, select = 1, shade = TRUE, seWithMean = TRUE, main = "LMA: (MAT)")
plot(m_lma, select = 2, shade = TRUE, seWithMean = TRUE, main = "LMA: (MAP)")

par(mfrow = c(1, 2))
plot(m_nmass, select = 1, shade = TRUE, seWithMean = TRUE, main = "Nmass: (MAT)")
plot(m_nmass, select = 2, shade = TRUE, seWithMean = TRUE, main = "Nmass: (MAP)")

par(mfrow = c(1, 1))

# --------------------------
# 7) Optional: save key outputs
# --------------------------
# Save model objects
saveRDS(m_lma,   file = "gamm_LMA_grid_species.rds")
saveRDS(m_nmass, file = "gamm_Nmass_grid_species.rds")

# Save summaries to text
capture.output(summary(m_lma),   file = "gamm_summary_LMA.txt")
capture.output(summary(m_nmass), file = "gamm_summary_Nmass.txt")
