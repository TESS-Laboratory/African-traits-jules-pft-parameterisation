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

# Keep only the columns we need ----
vars <- c("AccSpeciesName", "TraitName", "StdValue", 
          "UnitName", "Latitude", "Longitude")

trait_workdata<- trait_data %>% dplyr::select(one_of(vars))


# Make a prelim plot ----

(prelim_plot <- ggplot(trait_workdata, aes(x = Longitude, y = Latitude, 
                                           colour = TraitName)) +
   geom_point())

## Further cleaning - Filter out rows with latitude and longitude outside of a specified range ----
trait_workdata <- trait_workdata %>%
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
    geom_point(data = trait_workdata,  
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
    geom_tile(data = trait_workdata, aes(x = Longitude, y = Latitude, fill = TraitName), width = 1, height = 1) +
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
trait_africa <- trait_workdata %>%
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





# Make separate plots for each trait in the African region ----
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




# Make Africa map of traits ----
ggplot(trait_africa) +
  aes(x = Longitude, y = Latitude) +
  geom_sf(
    data = africa, aes(geometry = geometry),
    fill = NA, color = "black", inherit.aes = FALSE, lwd = 0.2
  ) + # Add country borders
  geom_hex(binwidth = 7, alpha=0.6) +
  scale_fill_viridis_c() +
  facet_wrap(~TraitName, ncol = 6, labeller = label_wrap_gen(width = 30)) +  # Adjust width as needed
  theme_classic() +
  theme(strip.text = element_text(size = 7, angle = 0, hjust = 0.9))  # Adjust size and angle as needed




