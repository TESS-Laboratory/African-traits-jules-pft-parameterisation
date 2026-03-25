# ==========================================================
# PAIRED MODEL PERFORMANCE COMPARISON
# GPP only
# - one shared legend
# - legend shows SHAPES only
# - hollow markers
# - 2-row layout
# - sites ordered by ecosystem type
# ==========================================================

library(tidyverse)
library(patchwork)

# ---------------------------
# Load statistics table
# ---------------------------
stats <- read_csv("option4_timeseries_heatmaps/performance_stats_all_variables.csv")

# Keep only GPP
stats <- stats %>%
  filter(variable == "GPP")

# ---------------------------
# Define ecosystem order
# ---------------------------
site_ecosystem <- tribble(
  ~site, ~ecosystem,
  "BW_GUM", "Wetland",
  "BW_NXR", "Wetland",
  "UG_JIN", "Wetland",
  
  "CG_TCH", "Grassland",
  "ML_AGG", "Grassland",
  "SD_DEM", "Grassland",
  "SN_DHR", "Grassland",
  "ZA_CATH", "Grassland",
  "ZA_WGN", "Grassland",
  
  "GH_ANK", "Forest",
  "ZM_MON", "Forest",
  
  "SN_NKR", "Cropland",
  "SN_RAG", "Cropland",
  
  "NE_WAM", "Cropland-Savanna",
  
  "NE_WAF", "Savanna-Cropland",
  
  "ZA_KRU", "Savanna"
)

ecosystem_order <- c(
  "Wetland",
  "Grassland",
  "Forest",
  "Cropland",
  "Cropland-Savanna",
  "Savanna-Cropland",
  "Savanna"
)

# ---------------------------
# Join ecosystem information
# ---------------------------
stats <- stats %>%
  left_join(site_ecosystem, by = "site")

site_levels <- stats %>%
  arrange(factor(ecosystem, levels = ecosystem_order), site) %>%
  pull(site) %>%
  unique()

stats <- stats %>%
  mutate(site = factor(site, levels = site_levels))

# ---------------------------
# Convert to long format
# ---------------------------
plot_data <- stats %>%
  select(site, model, cor, rmse, bias) %>%
  pivot_longer(
    cols = c(cor, rmse, bias),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      cor = "Correlation",
      rmse = "RMSE",
      bias = "Bias"
    ),
    model = recode(
      model,
      Default = "Default",
      Reparam = "Reparameterized"
    )
  )

# ---------------------------
# Plot function
# ---------------------------
make_metric_plot <- function(metric_name, show_legend = FALSE) {
  
  df <- plot_data %>%
    filter(metric == metric_name)
  
  p <- ggplot(df, aes(x = site, y = value, group = site)) +
    
    geom_line(color = "grey75", linewidth = 0.5) +
    
    # Default = hollow circle
    geom_point(
      data = df %>% filter(model == "Default"),
      aes(shape = "Default"),
      size = 3.5,
      stroke = 1.3,
      color = "#2C7FB8",
      fill = NA
    ) +
    
    # Reparameterized = hollow triangle
    geom_point(
      data = df %>% filter(model == "Reparameterized"),
      aes(shape = "Reparameterized"),
      size = 3.8,
      stroke = 1.3,
      color = "#D95F02",
      fill = NA
    ) +
    
    scale_shape_manual(
      values = c(
        "Default" = 21,
        "Reparameterized" = 24
      )
    ) +
    
    guides(
      shape = guide_legend(
        override.aes = list(
          color = "black",
          fill = NA,
          size = 4,
          stroke = 1.2
        )
      )
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 18, face = "bold"),
      axis.text.y = element_text(size = 16, face = "bold"),
      axis.title = element_text(size = 18, face = "bold"),
      panel.grid.minor = element_blank(),
      plot.title = element_blank(),
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 20, face = "bold")
    ) +
    labs(
      x = "Site",
      y = metric_name
    )
  
  p
}

# ---------------------------
# Create panels
# Only ONE panel keeps the legend
# ---------------------------
p_cor  <- make_metric_plot("Correlation", show_legend = TRUE)
p_rmse <- make_metric_plot("RMSE", show_legend = FALSE)
p_bias <- make_metric_plot("Bias", show_legend = FALSE)

# ---------------------------
# Combine with one shared collected legend
# ---------------------------
final_plot <- ((p_cor | p_rmse) /
                 (p_bias | plot_spacer())) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 20, face = "bold")
  )

# ---------------------------
# Save
# ---------------------------
ggsave(
  filename = "Model_Performance_Comparison_GPP.png",
  plot = final_plot,
  width = 20,
  height = 18,
  dpi = 300
)

# Show plot
final_plot
