# -----------------------------
# To use this script - continuation from Combined_Traits scripts
# as the df used here are created in that script


library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(readr)

# 1. PFT order -----------------------------
pft_order <- c("BET-Tr", "BET-Te", "BDT", "NET", "C3", "C4", "ESH", "DSH")

# 2. JULES values -----------------------------
jules_df <- tibble::tribble(
  ~PFT,     ~LMA,   ~Nmass,
  "BET-Tr", 0.1039, 0.0170,
  "BET-Te", 0.1403, 0.0144,
  "BDT",    0.0823, 0.0210,
  "NET",    0.2263, 0.0115,
  "C3",     0.0498, 0.0219,
  "C4",     0.1370, 0.0113,
  "ESH",    0.1515, 0.0136,
  "DSH",    0.0550, 0.0238
)

# 3. Optional check: inspect trait names and units first -----------------------------
dplyr::count(Trait_species_with_PFT, TraitName, UnitName, sort = TRUE)
dplyr::count(Global_Trait_species_with_PFT, TraitName, UnitName, sort = TRUE)

# 4. Helper for quantiles -----------------------------
qfun <- function(x, p) {
  unname(stats::quantile(x, probs = p, na.rm = TRUE, type = 7))
}

# 5. Build PFT-level summaries from raw data -----------------------------
make_trait_summary <- function(df, source_name) {
  df %>%
    dplyr::mutate(
      trait_key = dplyr::case_when(
        stringr::str_detect(stringr::str_to_lower(TraitName), "mass per area") ~ "LMA",
        stringr::str_detect(stringr::str_to_lower(TraitName), "nitrogen") &
          stringr::str_detect(stringr::str_to_lower(TraitName), "dry mass") ~ "Nmass",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(trait_key), PFT %in% pft_order) %>%
    dplyr::group_by(PFT, trait_key) %>%
    dplyr::summarise(
      n      = sum(!is.na(StdValue)),
      q10    = qfun(StdValue, 0.10),
      q25    = qfun(StdValue, 0.25),
      median = median(StdValue, na.rm = TRUE),
      q75    = qfun(StdValue, 0.75),
      q90    = qfun(StdValue, 0.90),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from  = trait_key,
      values_from = c(n, q10, q25, median, q75, q90),
      names_sep   = "_"
    ) %>%
    dplyr::mutate(source = source_name)
}

africa_sum <- make_trait_summary(Trait_species_with_PFT, "Africa TRY")
global_sum <- make_trait_summary(Global_Trait_species_with_PFT, "Global TRY")

# 6. JULES in same structure -----------------------------
jules_sum <- jules_df %>%
  dplyr::transmute(
    PFT,
    n_LMA = NA_real_,
    q10_LMA = NA_real_,
    q25_LMA = NA_real_,
    median_LMA = LMA,
    q75_LMA = NA_real_,
    q90_LMA = NA_real_,
    n_Nmass = NA_real_,
    q10_Nmass = NA_real_,
    q25_Nmass = NA_real_,
    median_Nmass = Nmass,
    q75_Nmass = NA_real_,
    q90_Nmass = NA_real_,
    source = "JULES"
  )

# 7. Read JULES counts file -----------------------------
# Assumes the file is in your current working directory
jules_counts <- readr::read_csv("Anna_H_count_PFT_traits.csv", show_col_types = FALSE)

jules_counts2 <- jules_counts %>%
  dplyr::rename(
    n_LMA = LMA,
    n_Nmass = NMASS
  ) %>%
  dplyr::mutate(
    PFT = dplyr::case_when(
      PFT %in% c("Esh", "ESH") ~ "ESH",
      PFT %in% c("DSh", "DSH", "Dsh") ~ "DSH",
      TRUE ~ PFT
    ),
    source = "JULES"
  ) %>%
  dplyr::filter(PFT %in% pft_order) %>%
  dplyr::select(PFT, source, n_LMA, n_Nmass)

# 8. Prepare plotting data -----------------------------
plot_df <- dplyr::bind_rows(africa_sum, global_sum, jules_sum) %>%
  dplyr::mutate(
    PFT = factor(PFT, levels = pft_order),
    source = factor(source, levels = c("Africa TRY", "Global TRY", "JULES"))
  )

spread_df <- plot_df %>%
  dplyr::filter(source != "JULES")

# 9. Build n-label dataframe -----------------------------
n_panel_df <- dplyr::bind_rows(
  africa_sum %>% dplyr::select(PFT, source, n_LMA, n_Nmass),
  global_sum %>% dplyr::select(PFT, source, n_LMA, n_Nmass),
  jules_counts2 %>% dplyr::select(PFT, source, n_LMA, n_Nmass)
) %>%
  dplyr::mutate(
    source_short = dplyr::case_when(
      source == "Africa TRY" ~ "A",
      source == "Global TRY" ~ "G",
      source == "JULES" ~ "J"
    ),
    source_short = factor(source_short, levels = c("A", "G", "J")),
    line = paste0(source_short, ": nL=", n_LMA, ", nN=", n_Nmass)
  ) %>%
  dplyr::arrange(PFT, source_short) %>%
  dplyr::group_by(PFT) %>%
  dplyr::summarise(
    label = paste(line, collapse = "\n"),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    PFT = factor(PFT, levels = pft_order),
    x = 0.365,
    y = 0.043
  )

# 10. Plot -----------------------------
p_pft_traits <- ggplot() +
  
  # outer spread: 10th to 90th percentile
  ggplot2::geom_segment(
    data = spread_df,
    aes(
      x = q10_LMA, xend = q90_LMA,
      y = median_Nmass, yend = median_Nmass,
      colour = source
    ),
    linewidth = 0.5,
    alpha = 0.45,
    show.legend = FALSE
  ) +
  ggplot2::geom_segment(
    data = spread_df,
    aes(
      x = median_LMA, xend = median_LMA,
      y = q10_Nmass, yend = q90_Nmass,
      colour = source
    ),
    linewidth = 0.5,
    alpha = 0.45,
    show.legend = FALSE
  ) +
  
  # inner spread: 25th to 75th percentile
  ggplot2::geom_segment(
    data = spread_df,
    aes(
      x = q25_LMA, xend = q75_LMA,
      y = median_Nmass, yend = median_Nmass,
      colour = source
    ),
    linewidth = 1.1,
    show.legend = FALSE
  ) +
  ggplot2::geom_segment(
    data = spread_df,
    aes(
      x = median_LMA, xend = median_LMA,
      y = q25_Nmass, yend = q75_Nmass,
      colour = source
    ),
    linewidth = 1.1,
    show.legend = FALSE
  ) +
  
  # Africa + Global median points
  ggplot2::geom_point(
    data = plot_df %>% dplyr::filter(source != "JULES"),
    aes(
      x = median_LMA,
      y = median_Nmass,
      colour = source,
      fill = source,
      shape = source
    ),
    size = 4.2,
    stroke = 1.2
  ) +
  
  # JULES point
  ggplot2::geom_point(
    data = plot_df %>% dplyr::filter(source == "JULES"),
    aes(
      x = median_LMA,
      y = median_Nmass,
      colour = source,
      shape = source
    ),
    size = 4.2,
    stroke = 1.2
  ) +
  
  # n labels
  ggplot2::geom_text(
    data = n_panel_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    vjust = 1,
    size = 7,
    lineheight = 1.0,
    colour = "black",
    face = "bold"
  ) +
  
  ggplot2::facet_wrap(~PFT, nrow = 4) +
  
  ggplot2::scale_colour_manual(
    breaks = c("Africa TRY", "Global TRY", "JULES"),
    values = c(
      "Africa TRY" = "forestgreen",
      "Global TRY" = "firebrick",
      "JULES"      = "blue3"
    )
  ) +
  
  ggplot2::scale_fill_manual(
    breaks = c("Africa TRY", "Global TRY", "JULES"),
    values = c(
      "Africa TRY" = "forestgreen",
      "Global TRY" = "firebrick",
      "JULES"      = NA
    )
  ) +
  
  ggplot2::scale_shape_manual(
    breaks = c("Africa TRY", "Global TRY", "JULES"),
    values = c(
      "Africa TRY" = 21,
      "Global TRY" = 24,
      "JULES"      = 4
    )
  ) +
  
  ggplot2::guides(
    fill = "none",
    shape = "none",
    colour = ggplot2::guide_legend(
      override.aes = list(
        shape    = c(21, 24, 4),
        fill     = c("forestgreen", "firebrick", NA),
        size     = c(4, 4, 4),
        stroke   = c(1.2, 1.2, 1.2),
        linetype = c(0, 0, 0)
      )
    )
  ) +
  
  ggplot2::labs(
    x = expression(LMA~(kg~m^{-2})),
    y = expression(Nmass~(g~g^{-1})),
    colour = NULL
  ) +
  
  ggplot2::theme_classic(base_size = 14) +
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold", size = 20),
    strip.background = ggplot2::element_rect(fill = "white", colour = "black", linewidth = 0.8),
    axis.title = ggplot2::element_text(face = "bold", size = 24),
    axis.text = ggplot2::element_text(colour = "black", size = 18, face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.spacing = grid::unit(1.1, "lines"),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )

p_pft_traits

ggplot2::ggsave(
  "new_p_pft_traits_with_n.png",
  plot = p_pft_traits,
  width = 14,
  height = 16,
  dpi = 300,
  bg = "white"
)
