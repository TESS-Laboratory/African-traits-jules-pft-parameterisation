# ------------------------------------------------------------
# Plot lai_gb from Default and Reparameterised JULES outputs
# ------------------------------------------------------------

library(ncdf4)
library(tidyverse)
library(stringr)
library(lubridate)

# ----------- User paths ---------------
ncd_folder_default <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/Default"
ncd_folder_reparam <- "C:/Users/efa206/OneDrive - University of Exeter/Desktop/JULES_Output/Reparam"

# Use all NetCDF files.
# If you only want part6 files, change this to: "^part6_.*\\.nc$"
file_pattern <- "^part6_.*\\.nc$"

# ------------------------------------------------------------
# Helper: convert CF-style NetCDF time units to POSIXct
# ------------------------------------------------------------
convert_cf_time <- function(time_vals, time_units) {
  
  unit <- stringr::str_extract(time_units, "^[A-Za-z]+")
  origin <- stringr::str_match(time_units, "since\\s+(.+)$")[, 2]
  origin <- stringr::str_trim(origin)
  
  # Try several common date-time formats
  ref_time <- lubridate::ymd_hms(origin, tz = "UTC", quiet = TRUE)
  if (is.na(ref_time)) ref_time <- lubridate::ymd_hm(origin, tz = "UTC", quiet = TRUE)
  if (is.na(ref_time)) ref_time <- lubridate::ymd(origin, tz = "UTC", quiet = TRUE)
  
  multiplier_seconds <- dplyr::case_when(
    unit %in% c("second", "seconds", "sec", "secs") ~ 1,
    unit %in% c("minute", "minutes", "min", "mins") ~ 60,
    unit %in% c("hour", "hours", "hr", "hrs") ~ 3600,
    unit %in% c("day", "days") ~ 86400,
    TRUE ~ NA_real_
  )
  
  if (is.na(multiplier_seconds)) {
    stop("Unsupported time unit in NetCDF file: ", time_units)
  }
  
  ref_time + lubridate::seconds(time_vals * multiplier_seconds)
}

# ------------------------------------------------------------
# Helper: parse site name from filename
# ------------------------------------------------------------
parse_site_name <- function(fname) {
  
  # First try your previous filename style, e.g. part6_BW_GUM-JULES...
  site1 <- stringr::str_match(fname, "^part\\d+_([^-]+)-JULES")[, 2]
  
  if (!is.na(site1)) {
    return(site1)
  }
  
  # Fallback: extract standard site code pattern, e.g. BW_GUM, ZA_CATH
  site2 <- stringr::str_extract(fname, "[A-Z]{2}_[A-Z0-9]+")
  
  return(site2)
}

# ------------------------------------------------------------
# Extract lai_gb from one NetCDF file
# ------------------------------------------------------------
extract_lai_gb <- function(file_path) {
  
  ncd <- ncdf4::nc_open(file_path)
  on.exit(ncdf4::nc_close(ncd))
  
  if (!"lai_gb" %in% names(ncd$var)) {
    stop("Variable 'lai_gb' not found in: ", basename(file_path))
  }
  
  lai_vals <- ncdf4::ncvar_get(ncd, "lai_gb")
  
  time_vals <- ncdf4::ncvar_get(ncd, "time")
  time_units <- ncdf4::ncatt_get(ncd, "time", "units")$value
  dates <- convert_cf_time(time_vals, time_units)
  
  # Identify time dimension robustly
  var_obj <- ncd$var[["lai_gb"]]
  dim_names <- purrr::map_chr(var_obj$dim, "name")
  time_dim <- which(dim_names == "time")
  
  if (length(time_dim) == 0) {
    # fallback: find dimension matching length of time vector
    dims <- dim(lai_vals)
    time_dim <- which(dims == length(time_vals))[1]
  }
  
  dims <- dim(lai_vals)
  
  if (is.null(dims)) {
    lai_gb <- lai_vals
  } else if (length(dims) == 1) {
    lai_gb <- lai_vals
  } else {
    # Move time dimension to first position, then average over remaining dims
    vals_time_first <- aperm(lai_vals, c(time_dim, setdiff(seq_along(dims), time_dim)))
    lai_mat <- matrix(vals_time_first, nrow = dim(vals_time_first)[1])
    lai_gb <- rowMeans(lai_mat, na.rm = TRUE)
    lai_gb[is.nan(lai_gb)] <- NA_real_
  }
  
  tibble::tibble(
    date = dates,
    lai_gb = lai_gb
  )
}

# ------------------------------------------------------------
# Extract lai_gb from all files in a folder
# ------------------------------------------------------------
extract_lai_folder <- function(folder, configuration) {
  
  files <- list.files(
    folder,
    pattern = file_pattern,
    full.names = TRUE
  )
  
  if (length(files) == 0) {
    stop("No NetCDF files found in: ", folder)
  }
  
  purrr::map_dfr(files, function(file) {
    
    site <- parse_site_name(basename(file))
    
    extract_lai_gb(file) %>%
      dplyr::mutate(
        site = site,
        configuration = configuration,
        file = basename(file)
      )
  })
}

# ------------------------------------------------------------
# Extract Default and Reparameterised lai_gb
# ------------------------------------------------------------
lai_gb_all <- dplyr::bind_rows(
  extract_lai_folder(ncd_folder_default, "Default"),
  extract_lai_folder(ncd_folder_reparam, "Reparameterised")
) %>%
  dplyr::filter(!is.na(site)) %>%
  dplyr::group_by(configuration, site, date) %>%
  dplyr::summarise(
    lai_gb = mean(lai_gb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lai_gb = dplyr::if_else(is.nan(lai_gb), NA_real_, lai_gb)
  )

# ------------------------------------------------------------
# Add ecosystem labels
# ------------------------------------------------------------
lai_gb_all <- lai_gb_all %>%
  dplyr::mutate(
    site_label = dplyr::recode(
      site,
      "BW_GUM"  = "BW_GUM - Wetland",
      "BW_NXR"  = "BW_NXR - Wetland",
      "CG_TCH"  = "CG_TCH - Grassland",
      "GH_ANK"  = "GH_ANK - Forest",
      "ML_AGG"  = "ML_AGG - Grassland",
      "NE_WAF"  = "NE_WAF - Savanna",
      "NE_WAM"  = "NE_WAM - Savanna",
      "SD_DEM"  = "SD_DEM - Grassland",
      "SN_DHR"  = "SN_DHR - Grassland",
      "SN_NKR"  = "SN_NKR - Cropland",
      "SN_RAG"  = "SN_RAG - Cropland",
      "UG_JIN"  = "UG_JIN - Wetland",
      "ZA_CATH" = "ZA_CATH - Grassland",
      "ZA_KRU"  = "ZA_KRU - Savanna",
      "ZA_WGN"  = "ZA_WGN - Grassland",
      "ZM_MON"  = "ZM_MON - Forest",
      .default = site
    )
  )

# ------------------------------------------------------------
# Plot lai_gb: Default vs Reparameterised
# ------------------------------------------------------------
p_lai_gb_compare <- ggplot(
  lai_gb_all,
  aes(x = date, y = lai_gb, colour = configuration)
) +
  geom_line(linewidth = 0.8, alpha = 0.9) +
  facet_wrap(~site_label, scales = "free", ncol = 3) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = "Date",
    y = "Grid-box LAI",
    colour = "JULES configuration"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(p_lai_gb_compare)

ggsave(
  "lai_gb_default_vs_reparameterised.png",
  p_lai_gb_compare,
  width = 18,
  height = 16,
  dpi = 300,
  bg = "white"
)

write_csv(
  lai_gb_all,
  "lai_gb_default_vs_reparameterised.csv"
)
