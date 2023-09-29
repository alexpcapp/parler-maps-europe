library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(sf)

#df <- read_delim("parler-videos-geocoded.csv", delim = ",")

world <- ne_countries(scale = "medium", returnclass = "sf") # Get world country polygon

metadata <- read_delim("metadata_EU.csv", delim = ",") %>% 
  st_as_sf(., coords = c("X", "Y"), crs = st_crs(world)) %>% 
  relocate(geometry)

##  Prepare for working with geographical data ----
europe_sf_data <- st_as_sf(metadata, coords = c("Longitude", "Latitude"), crs = st_crs(world)) # Map dataframe on to 'world'-polygon according to coordinates.
europe_data <- st_make_valid(europe_sf_data)  # Make invalid geometry valid for data.
world <- st_make_valid(world) # Make the world polygon valid as well (just to make sure).


united_kingdom <- europe_data %>%
  filter(country == "United Kingdom")

uk_map <- ne_countries(country = "United Kingdom", returnclass = "sf", scale = "large")

ggplot() +
  geom_sf(data = uk_map) +
  geom_sf(data = united_kingdom, color = 'red', size = .5) +
  #ggtitle("Locations in the UK") +
  coord_sf(xlim = c(-8, 2),
           ylim = c(50, 59))+
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave("united_kingdom.png", dpi = 500)
