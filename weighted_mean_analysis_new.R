library(tidyverse)

# ---------------------------
# Load performance table
# ---------------------------
stats <- read_csv(
  "option4_timeseries_heatmaps/performance_stats_all_variables.csv",
  show_col_types = FALSE
) %>%
  filter(variable == "GPP")

# ---------------------------
# Site years
# ---------------------------
site_years <- tribble(
  ~site,     ~years,
  "BW_GUM",   3,
  "BW_NXR",   3,
  "CG_TCH",   4,
  "GH_ANK",   4,
  "ML_AGG",   4,
  "NE_WAF",  13.5,
  "NE_WAM",  13.5,
  "SD_DEM",   5,
  "SN_RAG",   3,
  "SN_DHR",  10,
  "SN_NKR",   4,
  "UG_JIN",   1,
  "ZA_CATH",  7,
  "ZA_KRU",  14,
  "ZA_WGN",  11,
  "ZM_MON",  10
)

# ---------------------------
# Join years and prepare metrics
# ---------------------------
stats_a <- stats %>%
  left_join(site_years, by = "site") %>%
  mutate(abs_bias = abs(bias))

if (any(is.na(stats_a$years))) {
  warning("Some sites are missing year information.")
}

# ---------------------------
# Long format for weighted means
# ---------------------------
long_a <- stats_a %>%
  dplyr::select(site, years, model, cor, rmse, abs_bias) %>%
  pivot_longer(
    cols = c(cor, rmse, abs_bias),
    names_to = "metric_key",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric_key,
      cor = "Correlation",
      rmse = "RMSE",
      abs_bias = "Absolute Bias"
    ),
    metric = factor(metric, levels = c("Correlation", "RMSE", "Absolute Bias"))
  )

# ---------------------------
# Weighted means by model
# ---------------------------
weighted_means_a <- long_a %>%
  group_by(metric, model) %>%
  summarise(
    weighted_mean = weighted.mean(value, w = years, na.rm = TRUE),
    total_weight = sum(years[!is.na(value)]),
    .groups = "drop"
  )

# ---------------------------
# Wide summary table
# Positive weighted_change = improvement
# ---------------------------
summary_table_a <- weighted_means_a %>%
  dplyr::select(metric, model, weighted_mean) %>%
  pivot_wider(names_from = model, values_from = weighted_mean) %>%
  mutate(
    weighted_change = case_when(
      metric == "Correlation"   ~ Reparam - Default,
      metric == "RMSE"          ~ Default - Reparam,
      metric == "Absolute Bias" ~ Default - Reparam
    ),
    interpretation = case_when(
      metric == "Correlation"   ~ "Positive = improved correlation",
      metric == "RMSE"          ~ "Positive = reduced RMSE",
      metric == "Absolute Bias" ~ "Positive = reduced absolute bias"
    )
  )

# View table
summary_table_a

# Optional save
write_csv(summary_table_a, "weighted_summary_approach_A.csv")

# ---------------------------
# Data for dumbbell plot
# ---------------------------
plot_df_a <- summary_table_a %>%
  mutate(y = 1)

plot_points_a <- plot_df_a %>%
  dplyr::select(metric, Default, Reparam) %>%
  pivot_longer(
    cols = c(Default, Reparam),
    names_to = "model",
    values_to = "value"
  ) %>%
  mutate(
    model = factor(model, levels = c("Default", "Reparam"))
  )

# ---------------------------
# Faceted dumbbell plot
# ---------------------------
p_a <- ggplot() +
  geom_segment(
    data = plot_df_a,
    aes(x = Default, xend = Reparam, y = 1, yend = 1),
    colour = "grey70",
    linewidth = 1
  ) +
  geom_point(
    data = plot_points_a %>% filter(model == "Default"),
    aes(x = value, y = 1, shape = model),
    size = 4,
    stroke = 1.2,
    colour = "#2C7FB8",
    fill = NA
  ) +
  geom_point(
    data = plot_points_a %>% filter(model == "Reparam"),
    aes(x = value, y = 1, shape = model),
    size = 4.3,
    stroke = 1.2,
    colour = "#D95F02",
    fill = NA
  ) +
  geom_text(
    data = plot_df_a,
    aes(
      x = (Default + Reparam) / 2,
      y = 1.10,
      label = paste0("Δ = ", round(weighted_change, 3))
    ),
    hjust = 0.5,
    size = 4.5,
    fontface = "bold"
  ) +
  facet_wrap(~metric, scales = "free_x", nrow = 1) +
  scale_y_continuous(
    limits = c(0.97, 1.13),
    breaks = NULL
  ) +
  scale_shape_manual(
    values = c("Default" = 21, "Reparam" = 24),
    labels = c("Default", "Reparameterized")
  ) +
  labs(
    x = "Weighted network mean (site-years weighted)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 13, face = "bold"),
    axis.title.x = element_text(size = 15, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "grey95", colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(10, 30, 10, 10)
  )

p_a

ggsave(
  "weighted_dumbbell_approach_A.png",
  p_a,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)
