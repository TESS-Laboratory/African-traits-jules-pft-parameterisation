# load libraries ----
library(tidyverse)
library(janitor)
library(scales)
library(patchwork)



# 1) Load and clean --------------------------

df <- read_csv("traits_with_MAT_MAP_ERA5Land_1991_2020_withLonLat.csv") %>%
  clean_names()

# Check column names quickly
names(df)

# Adjust these names if yours differ after clean_names()
# Expected: trait_name, std_value, mat_c, map_mm
df1 <- df %>%
  filter(!is.na(trait_name), !is.na(std_value), !is.na(mat_c), !is.na(map_mm)) %>%
  mutate(trait_name = as.factor(trait_name))



 
# 2) Plotting theme  --------------------------

theme_clean_panel <- function(base_size = 18) {
  theme_classic(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.9),
      
      axis.title = element_text(face = "bold", size = base_size + 4),
      axis.text  = element_text(size = base_size - 3),
      
      legend.title = element_text(face = "bold", size = base_size + 2),
      legend.text  = element_text(size = base_size),
      legend.key.height = unit(1.2, "cm"),
      legend.key.width  = unit(0.5, "cm"),
      
      plot.margin = margin(10, 15, 10, 10)
    )
}



 
# 3) Plot A: MAT–MAP density (climate envelope)--------------------------

n_bins_hex <- 70

p_climate <- ggplot(df1, aes(x = mat_c, y = map_mm)) +
  geom_hex(bins = n_bins_hex) +
  scale_y_continuous(labels = comma) +
  labs(x = "MAT (°C)", y = "MAP (mm yr⁻¹)", fill = "count") +
  theme_clean_panel(base_size = 18) 

p_climate


ggsave(
  filename = "Fig_climate_envelope_hex.png",
  plot = p_climate,
  width = 8, height = 6, units = "in", dpi = 400, bg = "white"
)








# 4) Helper: Trait vs MAT / MAP plots --------------------------

plot_trait_vs <- function(trait_nm, xvar = c("mat_c", "map_mm")) {
  xvar <- match.arg(xvar)
  
  xlab <- if (xvar == "mat_c") "MAT (°C)" else "MAP (mm yr⁻¹)"
  
  ggplot(df1 %>% filter(trait_name == trait_nm),
         aes(x = .data[[xvar]], y = std_value)) +
    geom_point(alpha = 0.35, size = 1) +
    geom_smooth(se = TRUE, method = "loess", span = 0.8) +
    scale_x_continuous(labels = if (xvar == "map_mm") comma else waiver()) +
    labs(x = xlab, y = paste0(trait_nm, " (std_value)")) +
    theme_clean_panel()
}




# 5) Plot 1–4: LMA & Nmass vs MAT and MAP --------------------------

p_lma_mat <- plot_trait_vs("Leaf Mass per Area", "mat_c")
p_lma_map <- plot_trait_vs("Leaf Mass per Area", "map_mm")

p_nmass_mat <- plot_trait_vs("Leaf nitrogen (N) content per leaf dry mass", "mat_c")
p_nmass_map <- plot_trait_vs("Leaf nitrogen (N) content per leaf dry mass", "map_mm")

p_lma_mat
p_lma_map
p_nmass_mat
p_nmass_map




# 6) log transform data for proper visualization ----

# ---- helper: log10 trait vs MAT/MAP (LOESS) ----
plot_trait_vs_log10 <- function(trait_nm,
                                xvar = c("mat_c", "map_mm"),
                                ylab = "Trait (log10)",
                                span = 0.8,
                                alpha = 0.25,
                                pt_size = 0.8,
                                base_size = 18) {
  
  xvar <- match.arg(xvar)
  xlab <- if (xvar == "mat_c") "MAT (°C)" else "MAP (mm yr\u207B\u00B9)"
  
  d <- df1 %>%
    filter(trait_name == trait_nm, !is.na(.data[[xvar]]), !is.na(std_value), std_value > 0) %>%
    mutate(y = log10(std_value))
  
  ggplot(d, aes(x = .data[[xvar]], y = y)) +
    geom_point(alpha = alpha, size = pt_size) +
    geom_smooth(se = TRUE, method = "loess", span = span) +
    scale_x_continuous(labels = if (xvar == "map_mm") comma else waiver()) +
    labs(x = xlab, y = ylab) +
    theme_clean_panel(base_size = base_size)
}





# ---- compute y-limits (comparable within each trait) ----
get_log10_rng <- function(trait_nm) {
  df1 %>%
    filter(trait_name == trait_nm, std_value > 0) %>%
    summarise(
      ymin = min(log10(std_value), na.rm = TRUE),
      ymax = max(log10(std_value), na.rm = TRUE)
    )
}



rng_lma   <- get_log10_rng("Leaf Mass per Area")
rng_nmass <- get_log10_rng("Leaf nitrogen (N) content per leaf dry mass")



# (optional) add a small padding so points/smooth don’t touch edges
pad_rng <- function(rng, pad = 0.05) {
  span <- rng$ymax - rng$ymin
  tibble(ymin = rng$ymin - pad * span, ymax = rng$ymax + pad * span)
}



rng_lma   <- pad_rng(rng_lma, pad = 0.06)
rng_nmass <- pad_rng(rng_nmass, pad = 0.06)



# ---- build plots ----
p_lma_mat <- plot_trait_vs_log10(
  "Leaf Mass per Area", "mat_c",
  ylab = "LMA (log10)"
) +
  coord_cartesian(ylim = c(rng_lma$ymin, rng_lma$ymax))


p_lma_map <- plot_trait_vs_log10(
  "Leaf Mass per Area", "map_mm",
  ylab = "LMA (log10)"
) +
  coord_cartesian(ylim = c(rng_lma$ymin, rng_lma$ymax))


p_nmass_mat <- plot_trait_vs_log10(
  "Leaf nitrogen (N) content per leaf dry mass", "mat_c",
  ylab = "Nmass (log10)"
) +
  coord_cartesian(ylim = c(rng_nmass$ymin, rng_nmass$ymax))


p_nmass_map <- plot_trait_vs_log10(
  "Leaf nitrogen (N) content per leaf dry mass", "map_mm",
  ylab = "Nmass (log10)"
) +
  coord_cartesian(ylim = c(rng_nmass$ymin, rng_nmass$ymax))



# Remove x-axis titles (top row)
p_lma_mat  <- p_lma_mat  + theme(axis.title.x = element_blank())
p_lma_map  <- p_lma_map  + theme(axis.title.x = element_blank())


# Remove y-axis titles (right column)
p_lma_map  <- p_lma_map  + theme(axis.title.y = element_blank())
p_nmass_map <- p_nmass_map + theme(axis.title.y = element_blank())



# ---- multi-panel (no title, no panel headings) ----
p_4panel <-
  (p_lma_mat | p_lma_map) /
  (p_nmass_mat | p_nmass_map)

p_4panel


ggsave("Fig_trait_vs_climate_4panel.png",
       p_4panel, width = 14, height = 9, units = "in", dpi = 400, bg = "white")


