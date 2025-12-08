##### This is same script as the other 2 combined, 
# only difference is I have merged multiple traits of LA and LMA regardless if there is petiole or not. 
# This has reduced the observation size from 16 to 7.

# safe keeping ----
#range(trait_africa$Latitude, na.rm = TRUE)
#range(trait_africa$Longitude, na.rm = TRUE)

#bad <- trait_africa %>%
#dplyr::filter(
# Latitude < -40 | Latitude > 40 |   # Africa-safe latitude window
#  Longitude < -25 | Longitude > 60     # Africa-safe longitude window
#  )

#bad %>% dplyr::select(AccSpeciesName, PFT, lat_col, lon_col) %>% head(50)


#load packages and data ----
library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
library(plotrix)
library(sf)
library(rnaturalearth)
library(lwgeom)
library(maps)
library(RColorBrewer) # not using this (atm)
library(tidyr)
library(tidyverse)
library(patchwork)
library(viridis)
library(rgeos)


# read data ----
trait_data_new <- read.csv("workdata_.csv")


# Specify the trait names you're interested in. there are other traits in this dataset i had to sort out
traits_to_remove <- c(
  "Leaf carbon/nitrogen (C/N) ratio",
  "Leaf carbon (C) content per leaf dry mass",
  "Leaf carbon (C) content per leaf area",
  "Leaf nitrogen/phosphorus (N/P) ratio",
  "Leaf phosphorus (P) content per leaf dry mass",
  "Leaf magnesium (Mg) content per leaf dry mass",
  "Leaf potassium (K) content per leaf dry mass",
  "Leaf sodium (Na) content per leaf dry mass",
  "Leaf calcium (Ca) content per leaf dry mass"
)

trait_data <- trait_data_new %>%
  filter(!TraitName %in% traits_to_remove)



# Filter out rows where StdValue is less than 0
trait_data <- trait_data %>%
  filter(StdValue > 0)



# Remove unknown species in the AccSpeciesName column
trait_data <- trait_data %>%
  filter(AccSpeciesName != "unknown")



# Keep only the columns we need ----
vars <- c("AccSpeciesName", "TraitName", "StdValue", 
          "UnitName", "Latitude", "Longitude", "Exposition", "Plant_dev_status", "Reference")

trait_workdata<- trait_data %>% dplyr::select(one_of(vars))


# Define a named vector with old trait names as names and corresponding new trait names as values
trait_name_mapping <- c(
  "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or excluded)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaf, petiole excluded)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaf, petiole included)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaflet, petiole excluded)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaflet, petiole included)" = "Leaf Area",
  "Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)" = "Leaf Area",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded" = "leaf area per leaf dry mass",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded" = "leaf area per leaf dry mass",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included" = "leaf area per leaf dry mass",
  "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or excluded" = "leaf area per leaf dry mass",
  "Photosynthesis carboxylation capacity (Vcmax) per leaf area (Farquhar model)" = "Vcmax/LA",
  "Photosynthesis carboxylation capacity (Vcmax) per leaf dry mass (Farquhar model)" = "Vcmax/LMA")

# Replace old trait names with new trait names
new_trait_workdata <- trait_workdata %>% 
  mutate(TraitName = ifelse(TraitName %in% names(trait_name_mapping), trait_name_mapping[TraitName], TraitName))




# converting units and changing units names to match JULES default unit for LMA (That is SLA=1/LMA) AND Nmass
# Step 1: Filter for the trait "leaf area per leaf dry mass"
leaf_area_mask <- new_trait_workdata$TraitName == "leaf area per leaf dry mass"

# Step 2: Convert SLA (mm2/mg) to LMA (kg/m2) by taking the reciprocal
new_trait_workdata$StdValue[leaf_area_mask] <- 1 / new_trait_workdata$StdValue[leaf_area_mask]

# Step 3: Update the UnitName for this trait to "kg/m2"
new_trait_workdata$UnitName[leaf_area_mask] <- "kg m-2"


# Step 1: Filter for the trait "Leaf nitrogen (N) content per leaf dry mass" and convert its values
# Replace the "TraitName" and "StdValue" columns with the actual column names in your dataset
leaf_nitrogen_mask <- new_trait_workdata$TraitName == "Leaf nitrogen (N) content per leaf dry mass"

# Step 2: Convert mg/g to kg/kg by dividing by 1000 for the corresponding StdValue column
new_trait_workdata$StdValue[leaf_nitrogen_mask] <- new_trait_workdata$StdValue[leaf_nitrogen_mask] / 1000

# Step 3: Update the UnitName for this specific trait to "kg/kg"
new_trait_workdata$UnitName[leaf_nitrogen_mask] <- "kg/kg"




# Specify the trait names you're interested in. 
# working with just Nmass and LMA  ## can comment this out if things change ##
traits_of_interest <- c(
  "leaf area per leaf dry mass",
  "Leaf nitrogen (N) content per leaf dry mass"
)

# Filter the data for the specified trait names and sort by TraitName
new_trait_workdata <- new_trait_workdata %>%
  filter(TraitName %in% traits_of_interest) %>%
  arrange(TraitName)


# compare the observed species for Global and Africa observation 
species_count_global <- new_trait_workdata %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



# compare the observed species for exposition status 
species_exposition <- new_trait_workdata %>%
  group_by(Exposition) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# compare the observed species for plant dev status 
species_dev_status <- new_trait_workdata %>%
  group_by(Plant_dev_status) %>%
  summarise(count = n()) %>%
  arrange(desc(count))





# 1st LEVEL ----

## Make a prelim plot ----

(prelim_plot <- ggplot(new_trait_workdata, aes(x = Longitude, y = Latitude, 
                                           colour = TraitName)) +
   geom_point())

## Further cleaning - Filter out rows with latitude and longitude outside of a specified range ----
new_trait_workdata <- new_trait_workdata %>%
  filter(Latitude <= 100)


## Make global map of traits ----
# this is for the future <- density of observations in a hex bin rather than the continuous  
#                           scalar palette as the information content of it is lost atm
#                           To better represent the tropics and Africa with less distortion in the global maps,  
#                           can you switch to using the a better projection 'Winkel Tripel' is the best (but even 'Robinson' is better than the 'Mercator').
#                           This is usually one line of extra code in the map script to change form the default.


# World map data in a Robinson projection
world <- ne_countries(scale = "medium", returnclass = "sf")
world_robinson <- st_transform(world, crs = "+proj=robin")

# Aggregate data by TraitName and geographic coordinates
data_aggregated <- new_trait_workdata %>%
  group_by(Latitude, Longitude, TraitName) %>%
  summarise(TraitCount = n()) %>%
  ungroup()

# Convert dataset to an sf object
data_sf <- st_as_sf(data_aggregated, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to Robinson projection
data_robinson <- st_transform(data_sf, crs = "+proj=robin")

# Plot the heat map with traits
global_map <- ggplot() +
  geom_sf(data = world_robinson, fill = "gray95", color = "gray80") +
  geom_sf(data = data_robinson, aes(color = TraitName, size = TraitCount), alpha = 0.7) +
  scale_color_viridis_d(
    option = "plasma", 
    name = "Trait",
    guide = guide_legend(
      override.aes = list(shape = 15, size = 5) # Use squares and adjust size
    )
  ) +
  scale_size_continuous(
    range = c(1, 15),  # Adjust point size range
    breaks = c(100, 1000, 3000, 5000, 7000, 10000, 15000, 20000),  # Specify custom breaks
    labels = c("100", "1k", "3k", "5k", "7k", "10k", "15k", "20k"),  # Format labels for clarity
    name = "Observation Count"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(color = NA),
    panel.grid = element_line(color = "gray80"),
    legend.position = "right",
    legend.text = element_text(size = 18),        # Increase size of legend text
    legend.title = element_text(size = 18)  # Increase size of legend title
  ) +
 # ggtitle("Global Distribution of Individual Plant Traits") +
  theme(
    text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )


ggsave("trait_global_map2.png", plot = global_map, width = 18, height = 10, dpi = 300, bg = "white")




# Plot the map
ggplot() +
  geom_sf(data = world_robinson, fill = "gray95", color = "gray80") +
  geom_sf(data = data_robinson, aes(color = TraitName), size = 2, alpha = 0.8) +
  scale_color_viridis_d(
    option = "plasma",
    name = "Trait Name",
    guide = guide_legend(
      override.aes = list(shape = 15, size = 5) # Square legend keys
    )
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid = element_line(color = "gray80"),
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  ) +
  ggtitle("Global Distribution of Plant Trait Observations") +
  theme(
    text = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )





# 2nd LEVEL  ---- 

## Make regional map of traits for Africa ----

# Get world map and filter for Africa

africa <- sf::st_make_valid(world[world$continent == "Africa", ])


# Filter trait data for Africa
trait_africa <- new_trait_workdata %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_filter(africa)

# Plot map
ggplot() +
  geom_sf(data = africa, fill = NA, color = "black", size = 0.5) +  # Add country borders
  geom_sf(data = trait_africa, aes(color = TraitName), size = 2, alpha = 0.8) +  # Plot points
  coord_sf() +
  scale_color_viridis_d(option = "plasma", name = "Trait") +  # Use a visually appealing color scale
  theme_void() +
  theme(
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold")
  ) +
  labs(x = "Longitude", y = "Latitude", color = "Trait") +
  guides(color = guide_legend(
    title = "Trait",
    override.aes = list(size = 4, alpha = 1)  # Enhance legend appearance
  )) +
  ggtitle("Plant Trait Observations in Africa") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))





#export both dataset
#write.csv(new_trait_workdata, "trait_data.csv", row.names = FALSE)
#write.csv(trait_africa, "trait_africa.csv", row.names = FALSE)




# compare the observed species for Africa observation 
species_count_africa <- trait_africa %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) 



# compare the observed species for exposition status 
species_exposition_africa <- trait_africa %>%
  group_by(Exposition) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


# compare the observed species for plant dev status 
species_dev_status_africa <- trait_africa %>%
  group_by(Plant_dev_status) %>%
  summarise(count = n()) %>%
  arrange(desc(count))



## Make separate plots for each trait in the African region ----
trait_plots_africa <- list()

# Define alphabet letters
letters <- LETTERS[1:length(unique(trait_africa$TraitName))]

for (i in seq_along(unique(trait_africa$TraitName))) {
  trait <- unique(trait_africa$TraitName)[i]
  trait_plot <- ggplot() +
    geom_sf(data = africa, fill = NA, color = "black") +
    geom_sf(data = filter(trait_africa, TraitName == trait), aes(color = TraitName)) +
    coord_sf() +
    theme_void() +
    labs(x = "Longitude", y = "Latitude", color = "Trait") +
    guides(color = guide_legend(title = paste(letters[i]))) +  # Set legend title
    ggtitle(letters[i]) +  # Use alphabet letters as titles
    theme(legend.position = "right")  # Position legend on the right
  
  trait_plots_africa[[as.character(trait)]] <- trait_plot
}

# Combine and display the plots on a single page with a common legend
trait_plots_africa_combined <- wrap_plots(trait_plots_africa, ncol = 4) +
  plot_layout(ncol = 4, guides = 'collect')  # Adjust the number of columns as needed

trait_plots_africa_combined




## Make Africa map of traits ----
Multi_trait_map <- ggplot(trait_africa) +
  aes(x = Longitude, y = Latitude) +
  geom_sf(
    data = africa, aes(geometry = geometry),
    fill = NA, color = "black", inherit.aes = FALSE, lwd = 0.2
  ) + # Add country borders
  geom_hex(binwidth = 2, alpha=0.8) +
  scale_fill_viridis_c() +
  facet_wrap(~TraitName, ncol = 4, labeller = label_wrap_gen(width = 20)) +  # Adjust width as needed
  theme_classic() +
  theme(strip.text = element_text(size = 15, angle = 0, hjust = 0.9))  # Adjust size and angle as needed


ggsave("Afri_trait_multi_map.png", plot = Multi_trait_map, width = 20, height = 12, dpi = 300, bg = "white")






# 3rd LEVEL ----

## Comparison between global and African observation-------------------

global_frequency <- table(new_trait_workdata$TraitName)
print(global_frequency)

# Frequency table for the filtered dataset with only African countries
africa_frequency <- table(trait_africa$TraitName)
print(africa_frequency)



## Convert frequency tables to data frames ----
global_frequency_df <- as.data.frame(global_frequency)
africa_frequency_df <- as.data.frame(africa_frequency)

# Rename columns for clarity
colnames(global_frequency_df) <- c("Trait", "Global_Frequency")
colnames(africa_frequency_df) <- c("Trait", "Africa_Frequency")

# Merge data frames based on the Trait column
merged_frequency <- merge(global_frequency_df, africa_frequency_df, by = "Trait", all = TRUE)



# Assuming merged_frequency$Trait is a factor, if not, convert it to a factor first
# merged_frequency$Trait_New <- factor(merged_frequency$Trait, levels = unique(merged_frequency$Trait_New)) 



## Plot the bar chart ----
barplot_comparison <- ggplot(merged_frequency, aes(x = Trait, y = Global_Frequency, fill = "Global")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = Africa_Frequency, fill = "Africa"), stat = "identity", position = "dodge") +
  labs(#title = "Trait Frequency Comparison",
       x = "Trait",
       y = "Frequency") +
  scale_fill_manual(values = c("Global" = "blue", "Africa" = "red")) +
  scale_y_continuous(labels = scales::comma_format()) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
        axis.text.y = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 16, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        legend.title = element_text(size = 18, face = "bold"),
        legend.text  = element_text(size = 16, face = "bold"))



ggsave("Barplot_comparison.png", plot = barplot_comparison, width = 20, height = 12, dpi = 300, bg = "white")





## The difference in percentage ----
# Calculate percentage differences
merged_frequency <- merged_frequency %>%
  mutate(Africa_Percentage = (Africa_Frequency / Global_Frequency) * 100)



# Create a pie chart with percentage of observation in Africa 
pie(merged_frequency$Africa_Percentage, labels = merged_frequency$Trait, 
    col = rainbow(length(merged_frequency$Trait)),
    main = "Percentage of Traits in Africa", cex = 0.8,
    label.pos = 0.5,  # Adjust the label position (e.g., 0.5 for diagonal)
    clockwise = TRUE)  # Set to TRUE for labels to appear clockwise


# visualize this as a bar chart
ggplot(merged_frequency, aes(x = Trait, y = Africa_Percentage, fill = "Africa")) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Global Traits in Africa",
       x = "Trait",
       y = "Percentage") +
  scale_fill_manual(values = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))



## visualizing this as a heat map plot for each one of the eight traits ----
# Create a heatmap for percentage differences
heat.data2 <- pivot_longer(data = merged_frequency,
                           cols = -c(1:3),
                           names_to = "Area",
                           values_to = "Africa_per")



(heat.map2 <- ggplot(data = heat.data2, mapping = aes(x = Trait,
                                                      y = Area,
                                                      fill = Africa_per)) +
    geom_tile() +
    xlab(label = "Traits") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))) 




# Create a heatmap for absolute frequencies

heat.data <- pivot_longer(data = merged_frequency,
                          cols = -c(1,4),
                          names_to = "Area",
                          values_to = "Abundance")


(heat.map <- ggplot(data = heat.data, mapping = aes(x = Trait,
                                                    y = Area,
                                                    fill = Abundance)) +
    geom_tile() +
    xlab(label = "Traits") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))




## Density ----
## Global and Africa density plots for each traits ----
# including N (The number of observation) for each data series plotted as annotations on the graph would greatly help with their interpretation.


# Plot density distribution for Africa
density_africa <- ggplot(trait_africa, aes(x = StdValue, fill = TRUE)) +
  geom_density() +
  facet_wrap(~TraitName, scales = "free", ncol = 4, labeller = label_wrap_gen(width = 30)) +
  labs(title = "Density Plot for Traits in Africa",
       x = "Standardized Value",
       y = "Density") +
  theme_minimal() + scale_x_log10() + 
  theme(legend.position = "none")


# Plot density distribution for global data
density_global <- ggplot(new_trait_workdata, aes(x = StdValue, fill = TRUE)) +
  geom_density() +
  facet_wrap(~TraitName, scales = "free", ncol = 4, labeller = label_wrap_gen(width = 30)) +
  labs(title = "Density Plot for Traits Globally",
       x = "Standardized Value",
       y = "Density") +
  theme_minimal() + scale_x_log10() +
  theme(legend.position = "none")




ggsave("density_africa.png", plot = density_africa, width = 20, height = 12, dpi = 300, bg = "white")
ggsave("density_global.png", plot = density_global, width = 20, height = 12, dpi = 300, bg = "white")





# Convert coordinates to match African CRS for global data
new_trait_workdata <- new_trait_workdata %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  sf::st_transform(st_crs(africa))  # Transform to match African data CRS

# Select only necessary columns in trait_africa
trait_africa <- trait_africa %>%
  dplyr::select(-Longitude, -Latitude)

# Add a column to indicate whether data is from Africa or not
trait_africa$Region <- "Africa"
new_trait_workdata$Region <- "Global"


# Combine Africa and global data
combined_data <- rbind(trait_africa, new_trait_workdata)

# Plot density distribution for combined data
density_combined <- ggplot(combined_data, aes(x = StdValue, fill = Region)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~TraitName, scales = "free", ncol = 4, labeller = label_wrap_gen(width = 30)) +
  labs(#title = "Density Plot for Traits: Africa vs Global",
       x = "Standardized Value",
       y = "Density",
       fill = "Region") +
  theme_minimal()+
  theme(strip.text = element_text(size = 15, angle = 0, hjust = 0.9)) + scale_x_log10()


ggsave("density_combined.png", plot = density_combined, width = 20, height = 12, dpi = 300, bg = "white")






# 4th LEVEL ----

# Data load and preparation ----

PFT_data <- read_csv("Mapped_PFT_Harmonized.csv")

Trait_species_data <- read_csv("trait_africa2.csv")


# Perform a left join to add PFT information to Trait_species
Trait_species_with_PFT <- Trait_species_data %>%
  dplyr::left_join(PFT_data %>%
                     dplyr::select(AccSpeciesName, PFT), 
                   by = "AccSpeciesName")



# omit NA data. these are species that could not be classified due to limited information and available resources

Trait_species_with_PFT <- Trait_species_with_PFT[!is.na(Trait_species_with_PFT$PFT), ]

# remove columns not useful 
Trait_species_with_PFT <- subset(Trait_species_with_PFT, select = -c(geometry))


# filter using `%in%` to exclude "in situ" but keep NA values
Trait_species_with_PFT <- Trait_species_with_PFT %>%
  filter(is.na(Exposition) | Exposition != "in situ")


## Visualization of data to ascertain the value of PFT classes in my data and what steps can be taken further ----
# 2. PFT Density Plot----
# This plot will visualize the density of PFTs in different regions. Could use a hexbin plot or kernel density estimation.

pft_counts <- Trait_species_with_PFT %>%
  count(PFT) %>%
  rename(count = n)


# Prepare custom legend labels
custom_labels <- pft_counts %>%
  mutate(label = paste(PFT, " (", count, " obs)", sep = ""))



(heat_map_density <- ggplot() +
    geom_hex(data = Trait_species_with_PFT, aes(x = Longitude, y = Latitude, fill = PFT), bins = 35) +
    geom_sf(data = africa, fill = NA, color = "black") +  # Add country borders
    coord_sf() +  # Use coord_sf instead of coord_quickmap
    theme_void() +
    labs(x = "Longitude", y = "Latitude", fill = "PFT") +
    scale_fill_viridis_d()) + 
  scale_fill_viridis_d(labels = custom_labels$label)



# Get PFT counts for the legend
pft_counts <- Trait_species_with_PFT %>%
  count(PFT) %>%
  rename(count = n)

# Create label with counts for each facet
Trait_species_with_PFT <- Trait_species_with_PFT %>%
  mutate(PFT_label = case_when(
    PFT == "BDT" ~ "Broadleaf Deciduous Trees",
    PFT == "BET-Tr" ~ "Tropical Broadleaf Evergreen Trees",
    PFT == "BET-Te" ~ "Tropical Broadleaf Temperate Trees",
    PFT == "C3" ~ "C3 Grasses",
    PFT == "C4" ~ "C4 Grasses",
    PFT == "DSH" ~ "Deciduous Shrubs",
    PFT == "ESH" ~ "Evergreen Shrubs",
    PFT == "NET" ~ "Needleleaf Evergreen Trees",
    TRUE ~ PFT
  ))





# Plot: facet by PFT_label
pft_density <- ggplot() +
  geom_hex(data = Trait_species_with_PFT, aes(x = Longitude, y = Latitude), bins = 25) +
  geom_sf(data = africa, fill = NA, color = "black", linewidth = 0.3) +
  coord_sf(xlim = c(-20, 55), ylim = c(-35, 37)) +
  facet_wrap(~PFT_label, ncol = 4) +
  scale_fill_viridis_c(option = "C", name = "No. of Observations", trans = "sqrt") +  # square-root scale evens extremes
  labs(
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )


ggsave("pft_density.png", plot = pft_density, width = 20, height = 12, dpi = 300, bg = "white")



# 3. Density Plot of Traits by PFT----
# allowing for a comparison of the distribution of trait values between different PFTs.

histogram_density2 <- ggplot(Trait_species_with_PFT, aes(x = StdValue, fill = PFT)) +
  geom_density(aes(y = after_stat(count)),
               bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ TraitName, scales = "free") +
  labs(x = "Standard Value") +
  theme_minimal() + scale_x_log10() +
  scale_fill_viridis_d() +
  theme(
    axis.text = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )


ggsave("histogram_density2.png", plot = histogram_density2, width = 20, height = 12, dpi = 300, bg = "white")



# better option: Histogram with y-axis = counts (how many observations)
histogram_density <- ggplot(Trait_species_with_PFT, aes(x = StdValue, fill = PFT)) +
  geom_histogram(aes(y = after_stat(count)),
                 bins = 30, position = "identity", alpha = 0.6) +
  facet_wrap(~ TraitName, scales = "free") +
  scale_x_log10() +
  scale_fill_viridis_d() +
  labs(x = "Trait value (log10 scale)", y = "Density") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15, face = "bold")
  )


ggsave("histogram_density.png", plot = histogram_density, width = 20, height = 12, dpi = 300, bg = "white")




# 4. PFT vs Traits----
# create faceted scatter plots or box plots to visualize the relationship between PFT and various traits.
# Help understand the variability and central tendency of each trait within each PFT

# Scatter plot for Standard Value by PFT, faceted by TraitName
ggplot(Trait_species_with_PFT, aes(x = PFT, y = StdValue, color = PFT)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ TraitName, scales = "free_y") +
  theme_minimal() +
  labs(title = "Standard Value vs. PFT by Trait", x = "PFT", y = "Standard Value") + 
  scale_y_log10() +
  theme(legend.position = "none")



# Box plot of Standard Value by PFT, faceted by TraitName
box_plot <- ggplot(Trait_species_with_PFT, aes(x = PFT, y = StdValue, fill = PFT)) +
  geom_boxplot() +
  facet_wrap(~ TraitName, scales = "free") +
  theme_minimal() +
  labs(x = "PFT", y = "trait value") + 
  scale_y_log10() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  theme(
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15, face = "bold")
  )

ggsave("box_plot.png", plot = box_plot, width = 20, height = 12, dpi = 300, bg = "white")





# 5. Trait Distributions by PFT----
# Histogram or density plots showing the distribution of each trait for different PFTs.

# Histogram
ggplot(Trait_species_with_PFT, aes(x = StdValue, fill = PFT)) +
  geom_histogram(bins = 30, position = "dodge", alpha = 0.7) +
  facet_wrap(~ TraitName, scales = "free") +
  theme_minimal() +
  labs(x = "Standard Value", y = "Count") +
  scale_fill_viridis_d()





# 6. Summary Statistics by PFT ----
# could also visualize summary statistics like means and standard deviations of traits by PFT using bar plots.

# Summary statistics
summary_stats <- Trait_species_with_PFT %>%
  group_by(PFT, TraitName) %>%
  summarise(mean = mean(StdValue, na.rm = TRUE),
            sd = sd(StdValue, na.rm = TRUE))



# Plot
geom_bar <- ggplot(summary_stats, aes(x = PFT, y = mean, fill = TraitName)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(x = "PFT", y = "Mean Value") +
  scale_fill_viridis_d() +
  theme(
    strip.text = element_text(size = 18, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15, face = "bold")
  )


ggsave("geom_bar.png", plot = geom_bar, width = 20, height = 12, dpi = 300, bg = "white")




# 7. Species diversity curve by PFT ----
# Prepare the data: Calculate the number of observations per PFT
pft_count <- Trait_species_with_PFT %>%
  group_by(PFT) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plotting: Rank PFTs on the x-axis and plot against their counts
diversity_curve <- ggplot(pft_count, aes(x = reorder(PFT, -count), y = count, group = 1)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +  # Adjust 'span' to control smoothness
  labs(#title = "Species accumulation curve",
    x = "PFT",
    y = "Observation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"))


ggsave("diversity_curve.png", plot = diversity_curve, width = 20, height = 12, dpi = 300, bg = "white")




# Calculate quantiles for the rank (x-axis) positions
quantiles <- quantile(1:nrow(pft_count), probs = c(0.25, 0.5, 0.75))

# Plotting: Rank PFTs on the x-axis and plot against their counts
ggplot(pft_count, aes(x = reorder(PFT, -count), y = count, group = 1)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +  # Smooth curve
  geom_vline(xintercept = quantiles, linetype = "dashed", color = "blue") +  # Vertical quantile lines
  labs(#title = "Species accumulation curve",
    x = "PFT",
    y = "Observation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# 8  Facet grid Trait distribution by PFT ----
# Summarize the data
trait_distribution <- Trait_species_with_PFT %>%
  group_by(PFT, TraitName) %>%
  summarise(count = n())

# Plot using facet grid
trait_bar <- ggplot(trait_distribution, aes(x = PFT, y = count, fill = TraitName)) +
  geom_bar(stat = "identity") +
  labs(#title = "Trait Distribution by PFT",
    x = "PFT",
    y = "Observations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        strip.text = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15, face = "bold")) +
  facet_wrap(~ TraitName, nrow = 3, ncol = 3) +
  scale_fill_viridis_d()

ggsave("trait_bar.png", plot = trait_bar, width = 20, height = 12, dpi = 300, bg = "white")



# 9. Calculate the mean or median of StdValue for each PFT and TraitName----
pft_lines <- Trait_species_with_PFT %>%
  group_by(PFT, TraitName) %>%
  summarise(mean_value = mean(StdValue, na.rm = TRUE))

# Plot with density and PFT lines
ggplot(Trait_species_with_PFT, aes(x = StdValue)) +
  geom_density(fill = "red", alpha = 0.3) +
  facet_wrap(~TraitName, scales = "free", ncol = 4, labeller = label_wrap_gen(width = 30)) +
  geom_vline(data = pft_lines, aes(xintercept = mean_value, color = PFT), linetype = "solid") +
  labs(#title = "Density Plot for Traits in Africa with PFT Lines",
    x = "Standardized Value",
    y = "Density") +
  theme_minimal() +
  scale_x_log10() +
  theme(legend.position = "bottom", panel.grid = element_blank())





# Calculate the summary statistics
summary_stats <- Trait_species_with_PFT %>%
  group_by(PFT, TraitName) %>%
  summarise(
    mean = mean(StdValue, na.rm = TRUE),
    median = median(StdValue, na.rm = TRUE),
    IQR = IQR(StdValue, na.rm = TRUE),
    min = min(StdValue, na.rm = TRUE),
    max = max(StdValue, na.rm = TRUE)
  )

# View the results
print(summary_stats)




write.csv(summary_stats, "summary_stats_new.csv", row.names = FALSE)



