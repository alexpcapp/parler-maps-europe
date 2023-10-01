library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(hrbrthemes)
library(extrafont)



#df <- read_delim("parler-videos-geocoded.csv", delim = ",")

world <- ne_countries(scale = "medium", returnclass = "sf") # Get world country polygon

metadata <- read_delim("metadata_EU.csv", delim = ",") %>% 
  st_as_sf(., coords = c("X", "Y"), crs = st_crs(world)) %>% 
  relocate(geometry) 

##  Prepare for working with geographical data ----
europe_sf_data <- st_as_sf(metadata, coords = c("Longitude", "Latitude"), crs = st_crs(world)) # Map dataframe on to 'world'-polygon according to coordinates.
europe_data <- st_make_valid(europe_sf_data)  %>%   # Make invalid geometry valid for data.
  mutate(country = if_else(country == "Czech Republic", "Czechia", country))
world <- st_make_valid(world) # Make the world polygon valid as well (just to make sure).

##########################

countries_list  <- unique(europe_data$country)

countries_with_map_units <- c("France", "Norway", "Netherlands")

# Loop through each unique country and create a separate plot ####
for (country_name in countries_list) {
  # Filter the dataset for the current country
  country_data <- europe_data[europe_data$country == country_name, ]


  # Check if the country requires "type = 'map_units'"
  if (country_name %in% countries_with_map_units) {
    # Get the map for the European part of the country
    country_map <- ne_countries(geounit = first(unique(country_data$country)), returnclass = "sf", scale = "large", type = "map_units")
    
    
    # Create the plot using the map
    ggplot() +
    geom_sf(data = country_map, fill = "white", color = "white") +
    geom_sf(data = country_data, 
          #color = "#ebcc34", 
          color = rgb(
            r = 181,
            g = 42,
            b = 72,
            max = 255
          ),
          alpha = 0.7, 
          size = 3, 
          shape = 16) +
  theme_ft_rc(
    base_family = "Arial",
    subtitle_family = "Arial",
    caption_family = "Arial"

  ) +
  theme(
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.justification = "top",
        legend.title = element_blank(),
        #panel.background = element_rect(fill = "lightblue"),
        #plot.background = element_rect(fill = "lightblue"),
        text=element_text(family="Arial")
        )  +
  labs(
    title = paste("Parler in ",unique(country_data$country)), 
    subtitle = paste("Parler videos with recording location in ", unique(country_data$country), " (N = ", nrow(country_data),").", sep = ""),
    caption = "\nData: https://ddosecrets.com/wiki/Parler"
  )
    
  } else {
    country_map <- ne_countries(geounit = first(unique(country_data$country)), returnclass = "sf", scale = "large")
    # Create the plot without "type = 'map_units'"
    ggplot() +
    geom_sf(data = country_map, fill = "white", color = "white") +
    geom_sf(data = country_data, 
          #color = "#ebcc34", 
          color = rgb(
            r = 181,
            g = 42,
            b = 72,
            max = 255
          ),
          alpha = 0.7, 
          size = 3, 
          shape = 16) +
  theme_ft_rc(
    base_family = "Arial",
    subtitle_family = "Arial",
    caption_family = "Arial"

  ) +
  theme(
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.justification = "top",
        legend.title = element_blank(),
        #panel.background = element_rect(fill = "lightblue"),
        #plot.background = element_rect(fill = "lightblue"),
        text=element_text(family="Arial")
        )  +
  labs(
    title = paste("Parler in ",unique(country_data$country)), 
    subtitle = paste("Parler videos with recording location in ", unique(country_data$country), " (N = ", nrow(country_data),").", sep = ""),
    caption = "\nData: https://ddosecrets.com/wiki/Parler"
  )
  }

  #current_plot <- plot_layout +
  #  ggtitle(paste("Country:", country))

  paste("Country name:", unique(countries_list))

  # Save the plot as a PNG file with the country name as the filename
  filename <- paste("maps/",tolower(gsub(" ", "-", country_name)), ".png", sep = "")
  #ggsave(filename, current_plot, width = 6, height = 4)  # Adjust width and height as needed
  ggsave(filename, dpi = 500)
}



