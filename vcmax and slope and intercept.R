# ============================================================
# FULL SCRIPT: Vcmax25 from Karina's formula + slope/intercept visual
# ============================================================

# Clean session (optional but helpful)
rm(list = ls())

# Packages
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1) Enter your data exactly as shared ----
df <- tibble(
  PFT = c("BET-Tr","BET-Te","BDT","NET","NDT","C3","C4","ESh","DSh"),
  
  LMA_JULES = c(0.1039, 0.1403, 0.0823, 0.2263, 0.1006, 0.0498, 0.1370, 0.1515, 0.0550),
  TRY_LMA_MEDIAN = c(0.0898, 0.1069, 0.0965, 0.0195, 0.1006, 0.0724, 0.0569, 0.1306, 0.0702),
  
  NMASS_JULES = c(0.0170, 0.0144, 0.0210, 0.0115, 0.0186, 0.0219, 0.0113, 0.0136, 0.0238),
  TRY_NMASS_MEDIAN = c(0.0250, 0.0152, 0.0237, 0.0110, 0.0186, 0.0242, 0.0170, 0.0114, 0.0260),
  
  Vint_JULES = c(7.21, 3.90, 5.73, 6.32, 6.32, 6.42, 0.00, 14.71, 14.71),
  Vsl_JULES  = c(19.22, 28.40, 29.81, 18.15, 23.79, 40.96, 20.48, 23.15, 23.15)
)

# ---- 2) Karina's formula (EXACT) ----
# vcmax25 = ( vint + vsl * nmass * lma * 1000.0 ) * 1.0E-6
# Units:
#   lma   = kg m-2
#   nmass = kg/kg
# Output:
#   mol m-2 s-1 (JULES-ready)
vcmax25_karina <- function(vint, vsl, nmass, lma) {
  (vint + vsl * nmass * lma * 1000.0) * 1.0e-6
}

# ---- 3) Compute Na and vcmax25 for JULES traits and TRY medians ----
out <- df %>%
  mutate(
    # Leaf nitrogen per area (g N m-2)
    Na_gm2_JULES = NMASS_JULES * LMA_JULES * 1000.0,
    Na_gm2_TRY   = TRY_NMASS_MEDIAN * TRY_LMA_MEDIAN * 1000.0,
    
    # vcmax25 from Karina formula (mol m-2 s-1)
    vcmax25_mol_JULES   = vcmax25_karina(Vint_JULES, Vsl_JULES, NMASS_JULES, LMA_JULES),
    vcmax25_mol_TRYmed  = vcmax25_karina(Vint_JULES, Vsl_JULES, TRY_NMASS_MEDIAN, TRY_LMA_MEDIAN),
    
    # Convert to umol m-2 s-1 for plotting/interpretation
    vcmax25_umol_JULES  = vcmax25_mol_JULES  * 1e6,
    vcmax25_umol_TRYmed = vcmax25_mol_TRYmed * 1e6
  )

# Print a quick table (optional)
print(out %>%
        dplyr::select(PFT, Na_gm2_JULES, vcmax25_umol_JULES, Na_gm2_TRY, vcmax25_umol_TRYmed))

# ---- 4) Make plotting datasets ----

# 4a) Points (JULES vs TRY)
plot_pts <- out %>%
  dplyr::select(PFT, Vint_JULES, Vsl_JULES,
                Na_gm2_JULES, Na_gm2_TRY,
                vcmax25_umol_JULES, vcmax25_umol_TRYmed) %>%
  pivot_longer(
    cols = c(Na_gm2_JULES, Na_gm2_TRY, vcmax25_umol_JULES, vcmax25_umol_TRYmed),
    names_to = c(".value", "source"),
    names_pattern = "(Na_gm2|vcmax25_umol)_(JULES|TRY|TRYmed)"
  ) %>%
  mutate(source = ifelse(source == "TRYmed", "TRY", source))

# 4b) Main line per PFT: vcmax25_umol = vint + vsl * Na
line_df <- out %>%
  rowwise() %>%
  do({
    p <- .
    na_min <- min(p$Na_gm2_JULES, p$Na_gm2_TRY) * 0.6
    na_max <- max(p$Na_gm2_JULES, p$Na_gm2_TRY) * 1.6
    Na_seq <- seq(na_min, na_max, length.out = 80)
    tibble(
      PFT = p$PFT,
      Na_gm2 = Na_seq,
      vcmax25_umol = p$Vint_JULES + p$Vsl_JULES * Na_seq
    )
  }) %>%
  ungroup()

# x-range per facet (for intercept segment & slope marker placement)
x_ranges <- line_df %>%
  group_by(PFT) %>%
  summarise(xmin = min(Na_gm2), xmax = max(Na_gm2), .groups = "drop")

# 4c) Intercept reference line (y=vint), drawn across the facet x-range
intercept_seg <- out %>%
  dplyr::select(PFT, Vint_JULES) %>%
  left_join(x_ranges, by = "PFT") %>%
  mutate(
    x = xmin, xend = xmax,
    y = Vint_JULES, yend = Vint_JULES
  )

# 4d) Slope "L" marker (rise/run = vsl over 1 g N m-2), anchored near left side
slope_marker <- out %>%
  dplyr::select(PFT, Vint_JULES, Vsl_JULES) %>%
  left_join(x_ranges, by = "PFT") %>%
  mutate(
    x0 = xmin + 0.08 * (xmax - xmin),
    run = 1.0,
    y0 = Vint_JULES + Vsl_JULES * x0,
    x1 = x0 + run,
    y1 = y0,
    x2 = x1,
    y2 = y0 + Vsl_JULES * run
  )

# ---- 5) Plot ----
p <- ggplot() +
  # main line
  geom_line(data = line_df, aes(Na_gm2, vcmax25_umol), linewidth = 0.8) +
  
  # intercept indicator (dashed horizontal line)
  geom_segment(
    data = intercept_seg,
    aes(x = x, y = y, xend = xend, yend = yend),
    linetype = "dashed",
    linewidth = 0.6
  ) +
  
  # slope marker: base (run)
  geom_segment(
    data = slope_marker,
    aes(x = x0, y = y0, xend = x1, yend = y1),
    linewidth = 0.7
  ) +
  # slope marker: rise
  geom_segment(
    data = slope_marker,
    aes(x = x1, y = y1, xend = x2, yend = y2),
    linewidth = 0.7
  ) +
  
  # points
  geom_point(data = plot_pts, aes(Na_gm2, vcmax25_umol, shape = source), size = 2.6) +
  
  facet_wrap(~PFT, scales = "free_x") +
  labs(
    x = expression(N[a]~"(g N m"^{-2}*")"),
    y = expression(V[cmax25]~"(µmol m"^{-2}~"s"^{-1}*")"),
    shape = ""
  ) +
  theme_bw()

print(p)
