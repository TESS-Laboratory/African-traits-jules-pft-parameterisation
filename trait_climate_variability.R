library(tidyverse)
library(janitor)
library(scales)



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

theme_clean_panel <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
}



 
# 3) Plot A: MAT–MAP density (climate envelope)--------------------------

n_bins_hex <- 70

p_climate <- ggplot(df1, aes(x = mat_c, y = map_mm)) +
  geom_hex(bins = n_bins_hex) +
  scale_y_continuous(labels = comma) +
  labs(x = "MAT (°C)", y = "MAP (mm yr⁻¹)", fill = "n") +
  theme_clean_panel() 

p_climate



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




## log transform dato for proper visualization ----

plot_trait_vs_log10 <- function(trait_nm, xvar = c("mat_c", "map_mm"),
                                span = 0.8, alpha = 0.35) {
  xvar <- match.arg(xvar)
  xlab <- if (xvar == "mat_c") "MAT (°C)" else "MAP (mm yr⁻¹)"
  
  ggplot(df1 %>% filter(trait_name == trait_nm, std_value > 0),
         aes(x = .data[[xvar]], y = log10(std_value))) +
    geom_point(alpha = alpha, size = 1) +
    geom_smooth(se = TRUE, method = "loess", span = span) +
    scale_x_continuous(labels = if (xvar == "map_mm") comma else waiver()) +
    labs(
      x = xlab,
      y = paste0("log10(", trait_nm, ")")
    ) +
    theme_clean_panel()
}




p_lma_mat_log  <- plot_trait_vs_log10("Leaf Mass per Area", "mat_c")
p_lma_map_log  <- plot_trait_vs_log10("Leaf Mass per Area", "map_mm")

p_nmass_mat_log <- plot_trait_vs_log10("Leaf nitrogen (N) content per leaf dry mass", "mat_c")
p_nmass_map_log <- plot_trait_vs_log10("Leaf nitrogen (N) content per leaf dry mass", "map_mm")

p_lma_mat_log
p_lma_map_log
p_nmass_mat_log
p_nmass_map_log










