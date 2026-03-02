# RQ1: Trait variability across MAT–MAP climate space (GAMM) 
# Traits: LMA + Nmass
# Response: log10(std_value)
# Fixed effects: smooth(MAT) + smooth(MAP)
# Random effects: grid_id (lat/lon-based) + species

# load libraries============================================================

library(tidyverse)
library(janitor)
library(mgcv)
library(scales)


# 1) Load + clean --------------------------

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
    grid_id = paste0(round(latitude, 1), "_", round(longitude, 1)),
    grid_id = as.factor(grid_id),
    
    # species random effect
    acc_species_name = as.factor(acc_species_name)
  )




# 2) Plot theme (optional) --------------------------
theme_clean_panel <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
}




# 3) Fit GAMM for one trait --------------------------
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



#(rough, ignore) -- d <- gam(trait_log10 ~ s(mat_c, k = k_mat) +
        # s(map_mm, k = k_map) + (1 | grid_id ) + (1 | species), data = dat, model = m)




# 4) Run models (LMA + Nmass) --------------------------
trait_lma   <- "Leaf Mass per Area"
trait_nmass <- "Leaf nitrogen (N) content per leaf dry mass"


fit_lma   <- fit_gamm_trait(trait_lma)
fit_nmass <- fit_gamm_trait(trait_nmass)


m_lma   <- fit_lma$model
m_nmass <- fit_nmass$model




# 5) Summaries + diagnostics --------------------------
summary(m_lma)
gam.check(m_lma)

summary(m_nmass)
gam.check(m_nmass)



# 6) Smooth plots (MAT and MAP effects) --------------------------
save_gam_smooth_2panel <- function(model, file_stub,
                                   ylab = "Partial effect",
                                   width_in = 10, height_in = 5,
                                   dpi = 400,
                                   rug = FALSE) {
  
  # ---- PDF ----
  pdf(paste0(file_stub, ".pdf"), width = width_in, height = height_in)
  par(mfrow = c(1, 2),
      mar = c(4.5, 5, 2, 1),
      cex.axis = 1.2,
      cex.lab = 1.3)
  
  plot(model, select = 1, shade = TRUE, seWithMean = TRUE,
       rug = rug,
       xlab = "MAT (°C)",
       ylab = ylab,
       main = "")
  
  plot(model, select = 2, shade = TRUE, seWithMean = TRUE,
       rug = rug,
       xlab = "MAP (mm yr\u207B\u00B9)",
       ylab = ylab,
       main = "")
  
  dev.off()
  
  # ---- PNG  ----
  png(paste0(file_stub, ".png"),
      width = width_in,
      height = height_in,
      units = "in",
      res = dpi)
  
  par(mfrow = c(1, 2),
      mar = c(4.5, 5, 2, 1),
      cex.axis = 1.2,
      cex.lab = 1.3)
  
  plot(model, select = 1, shade = TRUE, seWithMean = TRUE,
       rug = rug,
       xlab = "MAT (°C)",
       ylab = ylab,
       main = "")
  
  plot(model, select = 2, shade = TRUE, seWithMean = TRUE,
       rug = rug,
       xlab = "MAP (mm yr\u207B\u00B9)",
       ylab = ylab,
       main = "")
  
  dev.off()
}

# Save LMA smooths
save_gam_smooth_2panel(
  model = m_lma,
  file_stub = "Fig_GAMM_LMA_smooths",
  ylab = "Partial effect on log10(LMA)",
  rug = FALSE
)

# Save Nmass smooths
save_gam_smooth_2panel(
  model = m_nmass,
  file_stub = "Fig_GAMM_Nmass_smooths",
  ylab = "Partial effect on log10(Nmass)",
  rug = FALSE
)

# Reset layout
par(mfrow = c(1, 1))




#----I just want to see the saved plots---
par(mfrow = c(1, 2))

plot(m_lma, select = 1, shade = TRUE, seWithMean = TRUE,
     rug = FALSE,
     xlab = "MAT (°C)",
     ylab = "Partial effect on log10(LMA)",
     main = "")

plot(m_lma, select = 2, shade = TRUE, seWithMean = TRUE,
     rug = FALSE,
     xlab = "MAP (mm yr⁻¹)",
     ylab = "Partial effect on log10(LMA)",
     main = "")

par(mfrow = c(1, 1))



par(mfrow = c(1, 2))

plot(m_nmass, select = 1, shade = TRUE, seWithMean = TRUE,
     rug = FALSE,
     xlab = "MAT (°C)",
     ylab = "Partial effect on log10(nmass)",
     main = "")

plot(m_nmass, select = 2, shade = TRUE, seWithMean = TRUE,
     rug = FALSE,
     xlab = "MAP (mm yr⁻¹)",
     ylab = "Partial effect on log10(nmass)",
     main = "")

par(mfrow = c(1, 1))



# save the diagnostic plots

png("LMA_diagnostics_4panel.png",
    width = 8, height = 8, units = "in", res = 400)

par(mfrow = c(2,2))

# 1) QQ plot
qq.gam(m_lma, main = "QQ plot of residuals")

# 2) Residuals vs linear predictor
plot(residuals(m_lma) ~ m_lma$linear.predictors,
     main = "Residuals vs. linear predictor",
     xlab = "Linear predictor",
     ylab = "Residuals")

# 3) Histogram
hist(residuals(m_lma),
     main = "Histogram of residuals",
     xlab = "Residuals")

# 4) Response vs fitted
plot(fitted(m_lma), m_lma$y,
     main = "Response vs. fitted values",
     xlab = "Fitted values",
     ylab = "Response")

dev.off()



png("Nmass_diagnostics_4panel.png",
    width = 8, height = 8, units = "in", res = 400)

par(mfrow = c(2,2))

# 1) QQ plot
qq.gam(m_nmass, main = "QQ plot of residuals")

# 2) Residuals vs linear predictor
plot(residuals(m_nmass) ~ m_nmass$linear.predictors,
     main = "Residuals vs. linear predictor",
     xlab = "Linear predictor",
     ylab = "Residuals")

# 3) Histogram
hist(residuals(m_nmass),
     main = "Histogram of residuals",
     xlab = "Residuals")

# 4) Response vs fitted
plot(fitted(m_nmass), m_nmass$y,
     main = "Response vs. fitted values",
     xlab = "Fitted values",
     ylab = "Response")

dev.off()




# 7) save key outputs --------------------------
# Save summaries to text
capture.output(summary(m_lma),   file = "gamm_summary_LMA.txt")
capture.output(summary(m_nmass), file = "gamm_summary_Nmass.txt")


capture.output(gam.check(m_lma),   file = "gamm_check_LMA.txt")
capture.output(gam.check(m_nmass), file = "gamm_check_Nmass.txt")
