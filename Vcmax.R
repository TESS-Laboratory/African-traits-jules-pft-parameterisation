# ------------------------------------------------------------
# Vcmax25 from JULES trait physiology formula (Karina's formula)
# vcmax25 = ( vint + vsl * nmass * lma * 1000.0 ) * 1.0E-6
#
# Units expected:
#   lma   = kg m-2
#   nmass = kg kg-1   (mass fraction of N in leaf dry mass)
# Output:
#   vcmax25_mol = mol m-2 s-1   (JULES-ready)
#   vcmax25_umol = umol m-2 s-1 (more interpretable for plotting)
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1) Enter data exactly  ----
df <- tibble(
  PFT = c("BET-Tr","BET-Te","BDT","NET","NDT","C3","C4","ESh","DSh"),
  
  LMA_JULES = c(0.1039, 0.1403, 0.0823, 0.2263, 0.1006, 0.0498, 0.1370, 0.1515, 0.0550),
  TRY_LMA_MEDIAN = c(0.0898, 0.1069, 0.0965, 0.0195, 0.1006, 0.0724, 0.0569, 0.1306, 0.0702),
  
  NMASS_JULES = c(0.0170, 0.0144, 0.0210, 0.0115, 0.0186, 0.0219, 0.0113, 0.0136, 0.0238),
  TRY_NMASS_MEDIAN = c(0.0250, 0.0152, 0.0237, 0.0110, 0.0186, 0.0242, 0.0170, 0.0114, 0.0260),
  
  Vint_JULES = c(7.21, 3.90, 5.73, 6.32, 6.32, 6.42, 0.00, 14.71, 14.71),
  Vsl_JULES  = c(19.22, 28.40, 29.81, 18.15, 23.79, 40.96, 20.48, 23.15, 23.15)
)

# ---- 2) Karina's formula as a function ----
# Returns vcmax25 in mol m-2 s-1 (JULES-ready)
vcmax25_karina <- function(vint, vsl, nmass, lma) {
  (vint + vsl * nmass * lma * 1000.0) * 1.0e-6
}

# ---- 3) Compute vcmax25 for JULES traits and TRY medians ----
out <- df %>%
  mutate(
    # JULES traits
    vcmax25_mol_JULES = vcmax25_karina(Vint_JULES, Vsl_JULES, NMASS_JULES, LMA_JULES),
    vcmax25_umol_JULES = vcmax25_mol_JULES * 1e6,  # convert back to umol for easy reading
    
    # TRY median traits (keeping same vint/vsl you supplied)
    vcmax25_mol_TRYmed = vcmax25_karina(Vint_JULES, Vsl_JULES, TRY_NMASS_MEDIAN, TRY_LMA_MEDIAN),
    vcmax25_umol_TRYmed = vcmax25_mol_TRYmed * 1e6
  )

print(out %>% dplyr::select(PFT,
                            vcmax25_umol_JULES, vcmax25_mol_JULES,
                            vcmax25_umol_TRYmed, vcmax25_mol_TRYmed))

# ---- 4) Optional: a "curve/line" plot per PFT ----
# Here we plot vcmax25 (umol) vs Na (g N m-2), where:
# Na = nmass*lma*1000 (this is exactly the same conversion inside Karina's formula)

plot_df <- out %>%
  transmute(
    PFT,
    Na_gm2_JULES = NMASS_JULES * LMA_JULES * 1000.0,
    Na_gm2_TRY   = TRY_NMASS_MEDIAN * TRY_LMA_MEDIAN * 1000.0,
    vcmax25_umol_JULES,
    vcmax25_umol_TRYmed,
    Vint_JULES, Vsl_JULES
  ) %>%
  pivot_longer(
    cols = c(Na_gm2_JULES, Na_gm2_TRY, vcmax25_umol_JULES, vcmax25_umol_TRYmed),
    names_to = c(".value","source"),
    names_pattern = "(Na_gm2|vcmax25_umol)_(JULES|TRY)"
  )

# Build line data for each PFT (same relation as Karina’s formula, just in umol units)
line_df <- out %>%
  rowwise() %>%
  do({
    p <- .
    na_min <- min(p$NMASS_JULES * p$LMA_JULES * 1000.0, p$TRY_NMASS_MEDIAN * p$TRY_LMA_MEDIAN * 1000.0) * 0.6
    na_max <- max(p$NMASS_JULES * p$LMA_JULES * 1000.0, p$TRY_NMASS_MEDIAN * p$TRY_LMA_MEDIAN * 1000.0) * 1.4
    Na_seq <- seq(na_min, na_max, length.out = 60)
    tibble(
      PFT = p$PFT,
      Na_gm2 = Na_seq,
      vcmax25_umol = p$Vint_JULES + p$Vsl_JULES * Na_seq
    )
  }) %>%
  ungroup()

ggplot() +
  geom_line(data = line_df, aes(Na_gm2, vcmax25_umol), linewidth = 0.8) +
  geom_point(data = plot_df, aes(Na_gm2, vcmax25_umol, shape = source), size = 2.6) +
  facet_wrap(~PFT, scales = "free_x") +
  labs(
    x = expression(N[a]~"(g N m"^{-2}*")"),
    y = expression(V[cmax25]~"(µmol m"^{-2}~"s"^{-1}*")"),
    shape = ""
  ) +
  theme_bw()
