################################################################################
# Purpose: Create maps for ILBBY 2020 third quarter update
# Input data: September 5, 2020 download from iNat
# Output: 
#   -Map of observations by county
#   -Map of species by county
# Date: Sept 5, 2020
################################################################################

library(tidyverse)
library(urbnmapr)
library(iNatTools)

statesil <- filter(states, state_name == 'Illinois')
countiesil <- filter(counties, state_name == 'Illinois')

ilbby20 <- read.csv('observations-104228.csv') %>%
    mutate(place_state_name = "Illinois",
           place_county_name = recode(place_county_name, 
                                      "DeWitt" = "De Witt", 
                                      "De Kalb" = "DeKalb",
                                      "La Salle" = "LaSalle"),
           place_county_name = paste(place_county_name, "County"))
    
obs_by_county <- ilbby20 %>%
    group_by(place_state_name, place_county_name) %>%
    count(place_state_name, place_county_name) %>%
    full_join(countiesil, by = c('place_state_name' = 'state_name', 
                               'place_county_name' = 'county_name')) %>%
    replace_na(list(n = 0)) %>%
    mutate(cat = cut(n, c(-1,0,9,99,999,9999,99999)))

species_by_county <- ilbby20 %>%
    distinct(place_state_name, place_county_name, scientific_name) %>%
    group_by(place_state_name, place_county_name) %>%
    count(place_state_name, place_county_name) %>%
    full_join(countiesil, by = c('place_state_name' = 'state_name', 
                           'place_county_name' = 'county_name')) %>% 
    replace_na(list(n = 0)) %>%
    mutate(cat = cut(n, c(-1,0,9,99,999,9999)))

################################################################################

obs_map <- obs_by_county %>%
    # Fill counties based on number of observations
    ggplot(mapping = aes(long, lat, group = group, fill = cat)) + 
    geom_polygon(color = NA) +
    # Add state border
    geom_polygon(data = statesil, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = countiesil, mapping = aes(long, lat, group = group),
                 fill = NA, color = "white", size = 0.05) +
    # Set map projection
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    # Adjust theme elements
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(),
          legend.background = element_rect(fill="gray90")) +
    # Select color palette, format legend
    scale_fill_manual(values= c("White", "#80c3ae", "#4fa68b", "#3c7e69", "#295648", "#162e27"), 
                      guide = "legend", 
                      name = element_blank(), 
                      labels = c("0","1-9", "10-99", "100-999", "1,000-9,999", "10,000+")) +
    # Add title
    labs(title = "Observations")

obs_map

################################################################################

species_map <- species_by_county %>%
    # Fill counties based on number of observations
    ggplot(mapping = aes(long, lat, group = group, fill = cat)) + 
    geom_polygon(color = NA) +
    # Add state border
    geom_polygon(data = statesil, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = countiesil, mapping = aes(long, lat, group = group),
                 fill = NA, color = "white", size = 0.05) +
    # Set map projection
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    # Adjust theme elements
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_blank(),
          legend.background = element_rect(fill="gray90")) +
    # Select color palette, format legend
    scale_fill_manual(values= c("White", "#4fa68b", "#3c7e69", "#295648", "#162e27"), 
                      guide = "legend", 
                      name = element_blank(),
                      labels = c("0","1-9", "10-99", "100-999", "1,000+")) +
    # Add title
    labs(title = "Species")

species_map