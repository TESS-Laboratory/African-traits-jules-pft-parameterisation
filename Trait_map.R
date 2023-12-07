#load packages and data ----

library(ggplot2)  # ggplot() fortify()
library(dplyr)  # %>% select() filter() bind_rows()
library(rgdal)  # readOGR() spTransform()
library(raster)  # intersect()
library(ggsn)  # north2() scalebar()
library(rworldmap)  # getMap()
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("rnaturalearth", quietly = TRUE)) install.packages("rnaturalearth")
if (!requireNamespace("lwgeom", quietly = TRUE)) install.packages("lwgeom")
if (!requireNamespace("maps", quietly = TRUE)) install.packages("maps")
library(sf)
library(rnaturalearth)
library(lwgeom)
library(maps)
library(RColorBrewer) # not using this (atm)


trait_data <- read.csv("workdata_traits.csv")
View(trait_data)

# Keep only the columns we need ----
vars <- c("AccSpeciesName", "TraitName", "StdValue", "Latitude",
          "Longitude")

trait_workdata<- trait_data %>% dplyr::select(one_of(vars))

## Check column names and content ----

View(trait_workdata)

# Make a prelim plot ----

(prelim_plot <- ggplot(trait_workdata, aes(x = Longitude, y = Latitude, 
                                           colour = TraitName)) +
   geom_point())

## Further cleaning - Filter out rows with latitude and longitude outside of a specified range ----
trait_workdata <- trait_workdata %>%
  filter(Latitude <= 100)


# Make global map of traits ----

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
africa <- world[world$continent == "Africa", ]

### Set the CRS for both datasets ----
africa <- st_set_crs(africa, st_crs(trait_data))


# Convert trait data to sf object
trait_sf <- st_as_sf(trait_data, coords = c("Longitude", "Latitude"))

# Filter trait data for points within Africa
trait_africa <- st_intersection(trait_sf, africa)


## Plot map with country borders and data points ----
(ggplot() +
   geom_sf(data = africa, fill = NA, color = "black") +
   geom_sf(data = trait_africa, aes(color = TraitName)) +
   coord_sf() +
   theme_void() +
   labs(x = "Longitude", y = "Latitude", color = "Trait") +
   guides(color = guide_legend(title = "Trait")))





