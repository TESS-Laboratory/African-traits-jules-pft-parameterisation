library(tidyverse)
library(patchwork)

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
# Join ecosystem + years
# ---------------------------
stats <- stats %>%
  left_join(site_ecosystem, by = "site") %>%
  left_join(site_years, by = "site") %>%
  mutate(abs_bias = abs(bias))

if (any(is.na(stats$years))) {
  warning("Some sites are missing year information.")
}

# ---------------------------
# Site order and y positions
# ---------------------------
site_levels <- stats %>%
  distinct(site, ecosystem) %>%
  arrange(factor(ecosystem, levels = ecosystem_order), site) %>%
  pull(site)

site_positions <- tibble(
  site = site_levels,
  y = rev(seq_along(site_levels))
)

stats <- stats %>%
  left_join(site_positions, by = "site")

# ---------------------------
# Group boundaries
# ---------------------------
group_data <- stats %>%
  distinct(site, ecosystem, y) %>%
  group_by(ecosystem) %>%
  summarise(
    ymin = min(y),
    ymax = max(y),
    .groups = "drop"
  ) %>%
  arrange(factor(ecosystem, levels = ecosystem_order))

boundary_data <- group_data %>%
  mutate(yint = ymin - 0.5) %>%
  slice(1:(n() - 1))

# ---------------------------
# TOP ROW DATA: site-level values
# using Absolute Bias so x-axis matches bottom row
# ---------------------------
plot_data_top <- stats %>%
  dplyr::select(site, y, years, model, cor, rmse, abs_bias) %>%
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
    metric = factor(metric, levels = c("Correlation", "RMSE", "Absolute Bias")),
    model = recode(
      model,
      Default = "Default",
      Reparam = "Reparameterized"
    ),
    model = factor(model, levels = c("Default", "Reparameterized"))
  )

seg_data_top <- plot_data_top %>%
  dplyr::select(site, y, metric, model, value) %>%
  pivot_wider(names_from = model, values_from = value)

# ---------------------------
# LEFT TABLE DATA
# ---------------------------
left_df <- stats %>%
  distinct(site, y, years) %>%
  arrange(desc(y)) %>%
  mutate(
    years_lab = ifelse(years %% 1 == 0, as.character(as.integer(years)), as.character(years))
  )

y_limits_top <- c(min(site_positions$y) - 0.5, max(site_positions$y) + 0.9)
y_header <- max(site_positions$y) + 0.55

left_table <- ggplot() +
  geom_hline(
    data = boundary_data,
    aes(yintercept = yint),
    colour = "grey85",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = 0.02, y = y_header,
    label = "Site",
    hjust = 0,
    fontface = "bold",
    size = 5
  ) +
  annotate(
    "text",
    x = 0.82, y = y_header,
    label = "Years",
    hjust = 1,
    fontface = "bold",
    size = 5
  ) +
  geom_text(
    data = left_df,
    aes(x = 0.02, y = y, label = site),
    hjust = 0,
    size = 5.1,
    fontface = "bold",
    colour = "grey25"
  ) +
  geom_text(
    data = left_df,
    aes(x = 0.82, y = y, label = years_lab),
    hjust = 1,
    size = 4.9,
    colour = "grey20"
  ) +
  scale_y_continuous(
    limits = y_limits_top,
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(xlim = c(0, 0.90), clip = "off") +
  theme_void(base_size = 14) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(20, 6, 6, 6)
  )

# ---------------------------
# BOTTOM ROW DATA: weighted means (Approach A)
# ---------------------------
long_a <- stats %>%
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

weighted_means_a <- long_a %>%
  group_by(metric, model) %>%
  summarise(
    weighted_mean = weighted.mean(value, w = years, na.rm = TRUE),
    .groups = "drop"
  )

summary_table_a <- weighted_means_a %>%
  dplyr::select(metric, model, weighted_mean) %>%
  pivot_wider(names_from = model, values_from = weighted_mean) %>%
  mutate(
    weighted_change = case_when(
      metric == "Correlation"   ~ Reparam - Default,
      metric == "RMSE"          ~ Default - Reparam,
      metric == "Absolute Bias" ~ Default - Reparam
    )
  )

# ---------------------------
# Shared x scales per metric column
# ---------------------------
metric_levels <- c("Correlation", "RMSE", "Absolute Bias")

scale_info <- lapply(metric_levels, function(m) {
  vals <- c(
    plot_data_top$value[plot_data_top$metric == m],
    summary_table_a$Default[summary_table_a$metric == m],
    summary_table_a$Reparam[summary_table_a$metric == m]
  )
  rng <- range(vals, na.rm = TRUE)
  span <- diff(rng)
  if (span == 0) span <- max(abs(rng), na.rm = TRUE) * 0.1 + 1e-6
  xlim <- c(rng[1] - 0.08 * span, rng[2] + 0.08 * span)
  
  # all three are non-negative in this integrated version
  xlim[1] <- max(0, xlim[1])
  
  breaks <- scales::pretty_breaks(n = 4)(xlim)
  list(xlim = xlim, breaks = breaks)
})
names(scale_info) <- metric_levels

# ---------------------------
# Plot builders
# ---------------------------
make_top_metric <- function(metric_name) {
  df_plot <- plot_data_top %>% filter(metric == metric_name)
  df_seg  <- seg_data_top %>% filter(metric == metric_name)
  xlim    <- scale_info[[metric_name]]$xlim
  breaks  <- scale_info[[metric_name]]$breaks
  
  ggplot() +
    geom_hline(
      data = boundary_data,
      aes(yintercept = yint),
      colour = "grey85",
      linewidth = 0.7
    ) +
    geom_segment(
      data = df_seg,
      aes(
        x = Default,
        xend = Reparameterized,
        y = y,
        yend = y
      ),
      colour = "grey75",
      linewidth = 0.6
    ) +
    geom_point(
      data = df_plot %>% filter(model == "Default"),
      aes(x = value, y = y, shape = model),
      size = 3.5,
      stroke = 1.2,
      colour = "#2C7FB8",
      fill = NA
    ) +
    geom_point(
      data = df_plot %>% filter(model == "Reparameterized"),
      aes(x = value, y = y, shape = model),
      size = 3.8,
      stroke = 1.2,
      colour = "#D95F02",
      fill = NA
    ) +
    facet_grid(. ~ metric) +
    scale_x_continuous(
      limits = xlim,
      breaks = breaks
    ) +
    scale_y_continuous(
      breaks = site_positions$y,
      labels = rep("", nrow(site_positions)),
      limits = y_limits_top,
      expand = expansion(mult = c(0, 0))
    ) +
    scale_shape_manual(
      values = c(
        "Default" = 21,
        "Reparameterized" = 24
      )
    ) +
    guides(shape = "none") +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 18, face = "bold"),
      strip.background = element_rect(fill = "grey95", colour = "black", linewidth = 0.8),
      
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
      
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      
      plot.margin = margin(15, 5, 0, 0)
    )
}

make_bottom_metric <- function(metric_name, show_legend = FALSE) {
  df <- summary_table_a %>% filter(metric == metric_name)
  
  pts <- df %>%
    dplyr::select(metric, Default, Reparam) %>%
    pivot_longer(
      cols = c(Default, Reparam),
      names_to = "model",
      values_to = "value"
    ) %>%
    mutate(
      model = recode(model, Reparam = "Reparameterized"),
      model = factor(model, levels = c("Default", "Reparameterized"))
    )
  
  xlim   <- scale_info[[metric_name]]$xlim
  breaks <- scale_info[[metric_name]]$breaks
  
  ggplot() +
    geom_segment(
      data = df,
      aes(x = Default, xend = Reparam, y = 1, yend = 1),
      colour = "grey70",
      linewidth = 1
    ) +
    geom_point(
      data = pts %>% filter(model == "Default"),
      aes(x = value, y = 1, shape = model),
      size = 4,
      stroke = 1.2,
      colour = "#2C7FB8",
      fill = NA
    ) +
    geom_point(
      data = pts %>% filter(model == "Reparameterized"),
      aes(x = value, y = 1, shape = model),
      size = 4.3,
      stroke = 1.2,
      colour = "#D95F02",
      fill = NA
    ) +
    geom_text(
      data = df,
      aes(
        x = (Default + Reparam) / 2,
        y = 1.12,
        label = paste0("Δ = ", round(weighted_change, 3))
      ),
      hjust = 0.5,
      size = 4.5,
      fontface = "bold"
    ) +
    scale_x_continuous(
      limits = xlim,
      breaks = breaks
    ) +
    scale_y_continuous(
      limits = c(0.97, 1.13),
      breaks = NULL
    ) +
    scale_shape_manual(
      values = c(
        "Default" = 21,
        "Reparameterized" = 24
      )
    ) +
    guides(
      shape = if (show_legend) {
        guide_legend(
          override.aes = list(
            colour = c("#2C7FB8", "#D95F02"),
            fill = NA,
            size = 4,
            stroke = 1.2
          )
        )
      } else {
        "none"
      }
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 13, face = "bold"),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
      
      legend.position = if (show_legend) "bottom" else "none",
      legend.title = element_blank(),
      legend.text = element_text(size = 14, face = "bold"),
      
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      
      plot.margin = margin(0, 5, 5, 0)
    )
}
# ---------------------------
# Build metric columns
# ---------------------------
top_cor  <- make_top_metric("Correlation")
top_rmse <- make_top_metric("RMSE")
top_ab   <- make_top_metric("Absolute Bias")

bot_cor  <- make_bottom_metric("Correlation", show_legend = FALSE)
bot_rmse <- make_bottom_metric("RMSE", show_legend = FALSE)
bot_ab   <- make_bottom_metric("Absolute Bias", show_legend = TRUE)

# ---------------------------
# Stack each column
# ---------------------------
col_left <- left_table / plot_spacer() +
  plot_layout(heights = c(8.8, 2.2))

col_cor <- top_cor / bot_cor +
  plot_layout(heights = c(8.8, 2.2))

col_rmse <- top_rmse / bot_rmse +
  plot_layout(heights = c(8.8, 2.2))

col_ab <- top_ab / bot_ab +
  plot_layout(heights = c(8.8, 2.2))

# ---------------------------
# Final combined figure
# ---------------------------
final_combined <- (col_left | col_cor | col_rmse | col_ab) +
  plot_layout(
    widths = c(2.1, 3.4, 3.4, 3.4),
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )

# Show
final_combined

# Save
ggsave(
  filename = "GPP_site_and_weighted_combined.png",
  plot = final_combined,
  width = 13,
  height = 13,
  dpi = 300,
  bg = "white"
)

