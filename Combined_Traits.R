##### This is same script as the other 2 combined, 
# only difference is I have merged multiple traits of LA and LMA regardless if there is petiole or not. 
# This has reduced the observation size from 16 to 7.


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


trait_data1 <- read_csv("workdata_traits.csv")

trait_data2 <- read_csv("workdata_traits2.csv")

trait_data <- bind_rows(trait_data1, trait_data2)


# Filter out rows where StdValue is less than 0
trait_data <- trait_data %>%
  filter(StdValue > 0)



# Remove unknown species in the AccSpeciesName column
trait_data <- trait_data %>%
  filter(AccSpeciesName != "unknown")



# Keep only the columns we need ----
vars <- c("AccSpeciesName", "TraitName", "StdValue", 
          "UnitName", "Latitude", "Longitude")

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


# compare the observed species for Global and Africa observation 
species_count_global <- new_trait_workdata %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


species_count_africa <- trait_africa %>%
  group_by(AccSpeciesName) %>%
  summarise(count = n()) %>%
  arrange(desc(count))





#------------------------

# Make a prelim plot ----

(prelim_plot <- ggplot(new_trait_workdata, aes(x = Longitude, y = Latitude, 
                                           colour = TraitName)) +
   geom_point())

## Further cleaning - Filter out rows with latitude and longitude outside of a specified range ----
new_trait_workdata <- new_trait_workdata %>%
  filter(Latitude <= 100)


# Make global map of traits ----
# this is for the future <- density of observations in a hex bin rather than the continuous  
#                           scalar palette as the information content of is lost atm
#                           To better represent the tropics and Africa with less distortion in the global maps,  
#                           can you switch to using the a better projection 'Winkel Tripel' is the best (but even 'Robinson' is better than the 'Mercator').
#                           This is usually one line of extra code in the map script to change form the default.


world <- getMap(resolution = "low")

(with_world <- ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    geom_point(data = new_trait_workdata,  
               aes(x = Longitude, y = Latitude, 
                   colour = TraitName)) +
    coord_quickmap() +  # Prevents stretching when resizing
    theme_classic() +  # Remove ugly grey background
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="Traits")))





# Convert world data to sf object
world_sf <- st_as_sf(world)

(heat_map <- ggplot() +
    geom_tile(data = new_trait_workdata, aes(x = Longitude, y = Latitude, fill = TraitName), width = 1, height = 1) +
    geom_sf(data = world_sf, fill = NA, color = "black") +  # Add country borders
    coord_sf() +  # Use coord_sf instead of coord_quickmap
    theme_classic() +
    labs(x = "Longitude", y = "Latitude", fill = "Traits") +
    scale_fill_viridis_d())  




# 2nd level # ---- 

## Make regional map of traits for Africa ----

## Get world map data and filter for African countries ----
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- sf::st_make_valid(world[world$continent == "Africa", ])
trait_africa <- trait_data %>% dplyr::select(one_of(vars))
trait_africa <- new_trait_workdata %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_filter(africa)

## Plot map with country borders and data points ----
(ggplot() +
   geom_sf(data = africa, fill = NA, color = "black") +
   geom_sf(data = trait_africa, aes(color = TraitName)) +
   coord_sf() +
   theme_void() +
   labs(x = "Longitude", y = "Latitude", color = "Trait") +
   guides(color = guide_legend(title = "Trait")))





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
ggplot(trait_africa) +
  aes(x = Longitude, y = Latitude) +
  geom_sf(
    data = africa, aes(geometry = geometry),
    fill = NA, color = "black", inherit.aes = FALSE, lwd = 0.2
  ) + # Add country borders
  geom_hex(binwidth = 4, alpha=0.6) +
  scale_fill_viridis_c() +
  facet_wrap(~TraitName, ncol = 4, labeller = label_wrap_gen(width = 20)) +  # Adjust width as needed
  theme_classic() +
  theme(strip.text = element_text(size = 15, angle = 0, hjust = 0.9))  # Adjust size and angle as needed




## for future me, I just want to highlight how I saved dta for the PFT classification
#current_directory <- getwd()
# Save the dataset to the current working directory
#write.csv(trait_africa, file.path(current_directory, "trait_africa.csv"), row.names = FALSE)







# 3rd LEVEL ----

## Comparison between global and African observation-------------------

global_frequency <- table(new_trait_workdata$TraitName)
print("Global Frequency Table:")
print(global_frequency)

# Frequency table for the filtered dataset with only African countries
africa_frequency <- table(trait_africa$TraitName)
print("African Countries Frequency Table:")
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
ggplot(merged_frequency, aes(x = Trait, y = Global_Frequency, fill = "Global")) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_bar(aes(y = Africa_Frequency, fill = "Africa"), stat = "identity", position = "dodge") +
  labs(title = "Trait Frequency Comparison",
       x = "Trait",
       y = "Frequency") +
  scale_fill_manual(values = c("Global" = "blue", "Africa" = "red")) +
  scale_y_continuous(labels = scales::comma_format()) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"))







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




# Density ------------------------------------------------------------------
## Global and Africa density plots for each traits --------------------------------------------------------------------------
# including N (The number of observation) for each data series plotted as annotations on the graph would greatly help with their interpretation.


# Plot density distribution for Africa
ggplot(trait_africa, aes(x = StdValue, fill = TRUE)) +
  geom_density() +
  facet_wrap(~TraitName, scales = "free", ncol = 4, labeller = label_wrap_gen(width = 30)) +
  labs(title = "Density Plot for Traits in Africa",
       x = "Standardized Value",
       y = "Density") +
  theme_minimal() + scale_x_log10()


# Plot density distribution for global data
ggplot(new_trait_workdata, aes(x = StdValue, fill = TRUE)) +
  geom_density() +
  facet_wrap(~TraitName, scales = "free", ncol = 4, labeller = label_wrap_gen(width = 30)) +
  labs(title = "Density Plot for Traits Globally",
       x = "Standardized Value",
       y = "Density") +
  theme_minimal() + scale_x_log10()




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
ggplot(combined_data, aes(x = StdValue, fill = Region)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~TraitName, scales = "free", ncol = 4, labeller = label_wrap_gen(width = 30)) +
  labs(title = "Density Plot for Traits: Africa vs Global",
       x = "Standardized Value",
       y = "Density",
       fill = "Region") +
  theme_minimal()+
  theme(strip.text = element_text(size = 15, angle = 0, hjust = 0.9)) + scale_x_log10()





