library(tidyverse)
library(janitor)
library(mgcv)
library(scales)

# --------------------------
# Load + prep
# --------------------------
df <- read_csv("traits_with_MAT_MAP_ERA5Land_1991_2020_withLonLat.csv") %>%
  clean_names()

df1 <- df %>%
  filter(!is.na(trait_name), !is.na(std_value), !is.na(mat_c), !is.na(map_mm)) %>%
  filter(std_value > 0) %>%                             # needed for log10
  mutate(
    trait_name = as.factor(trait_name),
    trait_log10 = log10(std_value),
    
    acc_species_name = as.factor(acc_species_name),
    # grid id to capture repeated sampling at the same location / ERA5 cell-ish
    grid_id = as.factor(paste0(round(latitude, 2), "_", round(longitude, 2)))
  )

# Traits (exact labels from your file)
trait_lma   <- "Leaf Mass per Area"
trait_nmass <- "Leaf nitrogen (N) content per leaf dry mass"


# --------------------------
# Fit all 4 models for one trait
# --------------------------
fit_models_for_trait <- function(trait_nm, k_mat = 10, k_map = 10) {
  
  dat <- df1 %>% filter(trait_name == trait_nm)
  
  # 1) GAM (no random effects)
  m1_gam <- gam(
    trait_log10 ~ s(mat_c, k = k_mat) + s(map_mm, k = k_map),
    data = dat, method = "REML"
  )
  
  # 2) GAMM species only
  m2_sp <- gam(
    trait_log10 ~ s(mat_c, k = k_mat) + s(map_mm, k = k_map) +
      s(acc_species_name, bs = "re"),
    data = dat, method = "REML"
  )
  
  # 3) GAMM grid only
  m3_grid <- gam(
    trait_log10 ~ s(mat_c, k = k_mat) + s(map_mm, k = k_map) +
      s(grid_id, bs = "re"),
    data = dat, method = "REML"
  )
  
  # 4) GAMM grid + species
  m4_grid_sp <- gam(
    trait_log10 ~ s(mat_c, k = k_mat) + s(map_mm, k = k_map) +
      s(grid_id, bs = "re") + s(acc_species_name, bs = "re"),
    data = dat, method = "REML"
  )
  
  list(
    data = dat,
    models = list(
      GAM = m1_gam,
      GAMM_species = m2_sp,
      GAMM_grid = m3_grid,
      GAMM_grid_species = m4_grid_sp
    )
  )
}


# --------------------------
# Extract comparison metrics
# --------------------------
model_metrics <- function(model_list) {
  # returns tibble with AIC, deviance explained, adj R^2, EDF of smooths
  purrr::imap_dfr(model_list, function(m, nm) {
    s <- summary(m)
    
    # pull EDF for the main smooths (if present)
    st <- s$s.table
    edf_mat <- if (!is.null(st) && any(grepl("^s\\(mat_c", rownames(st)))) st[grep("^s\\(mat_c", rownames(st))[1], "edf"] else NA
    edf_map <- if (!is.null(st) && any(grepl("^s\\(map_mm", rownames(st)))) st[grep("^s\\(map_mm", rownames(st))[1], "edf"] else NA
    
    tibble(
      model = nm,
      AIC = AIC(m),
      dev_expl = s$dev.expl,
      adj_r2 = s$r.sq,
      edf_mat = edf_mat,
      edf_map = edf_map
    )
  }) %>% arrange(AIC)
}



# --------------------------
# Smooth plots helper
# --------------------------
plot_smooths <- function(model_list, title_prefix = "") {
  # Quick visual: MAT smooth and MAP smooth across models
  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  
  par(mfrow = c(length(model_list), 2), mar = c(4, 4, 2, 1))
  
  for (nm in names(model_list)) {
    m <- model_list[[nm]]
    plot(m, select = 1, shade = TRUE, seWithMean = TRUE,
         main = paste0(title_prefix, nm, " : s(MAT)"))
    plot(m, select = 2, shade = TRUE, seWithMean = TRUE,
         main = paste0(title_prefix, nm, " : s(MAP)"))
  }
}



# --------------------------
# Run for LMA
# --------------------------
fit_lma <- fit_models_for_trait(trait_lma)
metrics_lma <- model_metrics(fit_lma$models)
metrics_lma

# optional: visually compare smooth shapes (opens base plots)
plot_smooths(fit_lma$models, title_prefix = "LMA | ")

# --------------------------
# Run for Nmass
# --------------------------
fit_nmass <- fit_models_for_trait(trait_nmass)
metrics_nmass <- model_metrics(fit_nmass$models)
metrics_nmass

plot_smooths(fit_nmass$models, title_prefix = "Nmass | ")


summary(fit_lma$models$GAMM_grid_species)
summary(fit_nmass$models$GAMM_grid_species)
