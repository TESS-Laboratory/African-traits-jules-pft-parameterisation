library(tidyverse)

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

# reverse so first group appears at the top
stats <- stats %>%
  mutate(site = factor(site, levels = rev(site_levels)))

# ---------------------------
# Convert to long format
# ---------------------------
plot_data <- stats %>%
  dplyr::select(site, model, cor, rmse, bias) %>%
  pivot_longer(
    cols = c(cor, rmse, bias),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      cor  = "Correlation",
      rmse = "RMSE",
      bias = "Bias"
    ),
    metric = factor(metric, levels = c("Correlation", "RMSE", "Bias")),
    model = recode(
      model,
      Default = "Default",
      Reparam = "Reparameterized"
    ),
    model = factor(model, levels = c("Default", "Reparameterized"))
  )

# ---------------------------
# Make segment data
# ---------------------------
seg_data <- plot_data %>%
  dplyr::select(site, metric, model, value) %>%
  pivot_wider(names_from = model, values_from = value)

# ---------------------------
# Optional: reference line for Bias = 0
# ---------------------------
ref_data <- tibble(
  metric = factor("Bias", levels = c("Correlation", "RMSE", "Bias")),
  xint = 0
)

# ---------------------------
# Plot
# ---------------------------
final_plot <- ggplot() +
  
  # zero reference for bias only
  geom_vline(
    data = ref_data,
    aes(xintercept = xint),
    linetype = "dashed",
    colour = "grey55",
    linewidth = 0.6
  ) +
  
  # connecting line between Default and Reparameterized
  geom_segment(
    data = seg_data,
    aes(
      x = Default,
      xend = Reparameterized,
      y = site,
      yend = site
    ),
    colour = "grey75",
    linewidth = 0.6
  ) +
  
  # Default = hollow circle
  geom_point(
    data = plot_data %>% filter(model == "Default"),
    aes(x = value, y = site, shape = model),
    size = 3.5,
    stroke = 1.2,
    colour = "#2C7FB8",
    fill = NA
  ) +
  
  # Reparameterized = hollow triangle
  geom_point(
    data = plot_data %>% filter(model == "Reparameterized"),
    aes(x = value, y = site, shape = model),
    size = 3.8,
    stroke = 1.2,
    colour = "#D95F02",
    fill = NA
  ) +
  
  facet_grid(. ~ metric, scales = "free_x") +
  
  scale_shape_manual(
    values = c(
      "Default" = 21,
      "Reparameterized" = 24
    )
  ) +
  
  guides(
    shape = guide_legend(
      override.aes = list(
        colour = "black",
        fill = NA,
        size = 4,
        stroke = 1.2
      )
    )
  ) +
  
  labs(
    x = NULL,
    y = NULL
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 18, face = "bold")
  )

# show
final_plot

# save
ggsave(
  filename = "Model_Performance_Comparison_GPP_horizontal.png",
  plot = final_plot,
  width = 12,
  height = 12,
  dpi = 300,
  bg = "white"
)
