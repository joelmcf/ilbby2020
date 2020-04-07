# Purpose: Create maps and charts for ILBBY 2020 first quarter update
# Input data: April 4, 2020 downloads from iNat
# Output: 
#   -Bar chart showing participation growth across years
#   -Map of observations by county
#   -Map of species by county
#   -Calendar heat maps of flowering observations for 2016-2020
# Date: April 4, 2020
#--------------------------------------------------------------

# Create bar chart showing ILBBY participation growth across years

# Load packages
library(tidyverse)
library(scales)

# Input Jan-March participation data (taken from iNat site)
q1part <- data.frame(
    Year = c('2016', '2017', '2018', '2019', '2020'),
    Users = c(49, 85, 128, 200, 434),
    Species = c(279, 342, 412, 484, 578),
    Observations = c(773, 1147, 1495, 2493, 3965))

# Convert data frame to long format        
q1part_long <- pivot_longer(q1part, -Year, names_to = "pvar")

# Reorder variables
q1part_long2 <- q1part_long %>%
    mutate(pvar = fct_rev(as_factor(pvar))) 
    
# Create bar chart
g <- ggplot(q1part_long2, aes(x = Year, y = value, fill = fct_rev(pvar)))
g + geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label = comma(value)), position=position_dodge(width=0.9), 
              vjust=-0.25, size=3.5, color = "black") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x=element_text(size = 10, color = "black"),
          axis.title.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = 'bottom', 
          legend.text = element_text(size = 10, color = "Black"),
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 4200)) + 
    scale_fill_manual(name = element_blank(), 
                      values = c("Observations" = "#61c9a8", 
                                 "Species" = "#428a73", 
                                 "Users" = "#295648")) +
    ggtitle("January-March ILBBY participation, 2016-2020")

#--------------------------------------------------------------

# Map observations by county

# Code uses the Urban Institute's urbnmapr package
#   (https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2)
# Load packages
library(tidyverse)
library(urbnmapr)

# Filter urbanmapr county and state files
states2 <- filter(states, state_name == 'Illinois')
counties2 <- filter(counties, state_name == 'Illinois')

# Import data (iNat download)
ilbby20 <- read.csv('observations-84136.csv')

# Summarize obs by county, create dataset for map
obs <- ilbby20 %>%
    # Create state name variable for later merge, correct county name errors
    mutate(place_state_name = "Illinois",
           place_county_name = recode(place_county_name, 
                                      "DeWitt" = "De Witt", 
                                      "De Kalb" = "DeKalb",
                                      "La Salle" = "LaSalle")) %>%
    # Create dummy variable, append "County" to county variable for later merge
    mutate(x = 1, place_county_name = paste(place_county_name, "County")) %>% 
    # Summarize number of obs per county
    group_by(place_state_name, place_county_name) %>%
    summarize(totobs = sum(x)) %>%
    # Join county-level summary table with urbanmapr county shapefile
    full_join(counties, by = c('place_state_name' = 'state_name', 
                                   'place_county_name' = 'county_name')) %>% 
    # Filter to inculde only Illinois counties
    filter(place_state_name == 'Illinois') %>%
    # Replace NA values with 0
    replace_na(list(totobs = 0)) %>%
    # Create map fill categories
    mutate(obscat = cut(totobs, c(-1,0,9,99,999,9999)))

# Create obs map
obs_map <- obs %>%
    # Fill counties based on number of observations
    ggplot(mapping = aes(long, lat, group = group, fill = obscat)) + 
    geom_polygon(color = NA) +
    # Add state border
    geom_polygon(data = states2, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = counties2, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000", size = 0.05) +
    # Set map projection
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    # Adjust theme elements
    theme(panel.background = element_blank(),
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
    scale_fill_manual(values= c("White", "#64b59b", "#4a9b81", "#3a7965", "#295648"), 
                           guide = "legend", 
                           name = "Research-grade \nplant observations", 
                           labels = c("0","1-9", "10-99", "100-999", "1,000+")) +
    # Add title
    labs(title = "Illinois Botanists Big Year 2020: observations by county")

obs_map

#--------------------------------------------------------------

# Map species by county

# Summarize species by county, create dataset for map
species <- ilbby20 %>%
    # Create state name variable for later merge, correct county name errors
    mutate(place_state_name = "Illinois",
           place_county_name = recode(place_county_name, 
                                      "DeWitt" = "De Witt", 
                                      "De Kalb" = "DeKalb",
                                      "La Salle" = "LaSalle")) %>%
    # Create dummy variable, append "County" to county variable for later merge
    mutate(x = 1, place_county_name = paste(place_county_name, "County")) %>% 
    # Summarize number of species per county
    group_by(place_state_name, place_county_name, scientific_name) %>%
    summarize(totobs = sum(x)) %>%
    mutate(y = 1) %>%
    group_by(place_state_name, place_county_name) %>%
    summarize(totspecies = sum(y)) %>% 
    # Join county-level summary table with urbanmapr county shapefile
    full_join(counties, by = c('place_state_name' = 'state_name', 
                                                 'place_county_name' = 'county_name')) %>%
    # Filter to inculde only Illinois counties
    filter(place_state_name == 'Illinois') %>%
    # Replace NA values with 0
    replace_na(list(totspecies = 0)) %>%
    # Create map fill categories
    mutate(speciescat = cut(totspecies, c(-1,0,9,99,999)))

# Create species map
species_map <- species %>%
    # Fill counties based on number of observations
    ggplot(mapping = aes(long, lat, group = group, fill = speciescat)) + 
    geom_polygon(color = NA) +
    # Add state border
    geom_polygon(data = states2, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = counties2, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000", size = 0.05) +
    # Set map projection
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    # Modify theme elements
    theme(panel.background = element_blank(),
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
    scale_fill_manual(values= c("White", "#64b59b", "#4a9b81", "#3a7965"),
                                    name = "Plant species with \nresearch-grade \nobservations", 
                                    labels = c("0", "1-9", "10-99", "100-999")) +
    # Add title
    labs(title = "Illinois Botanists Big Year 2020: species by county")

species_map 
#--------------------------------------------------------------

# Calendar heat map of flowering observations

# Install tools to pull data
# install_github("pjhanly/iNatTools") 
library(iNatTools)
# Help file for iNat tool: https://pjhanly.github.io/iNat/iNat.html

# Install tools for calendar heat map
# Background: http://www.columbia.edu/~sg3637/blog/Time_Series_Heatmaps.html
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")

# Create teal color scheme
lt2dt <- c("#64b59b", "#295648")

# Pull data for obs of flowering plants
ilbby16f <-iNat(project = "illinois-botanists-big-year-2016", term_id = 12, 
                 term_value_id = 13)
ilbby17f <-iNat(project = "illinois-botanists-big-year-2017", term_id = 12, 
                term_value_id = 13)
ilbby18f <-iNat(project = "illinois-botanists-big-year-2018", term_id = 12, 
                term_value_id = 13)
ilbby19f <-iNat(project = "illinois-botanists-big-year-2019", term_id = 12, 
                term_value_id = 13)
ilbby20f <-iNat(project = "illinois-botanists-big-year-2020", term_id = 12, 
                term_value_id = 13)

# Create datasets showing total number of observations on each date
obs_by_day16 <- ilbby16f %>%
    mutate(x = 1) %>%
    group_by(observed_on) %>%
    summarize(totobs = sum(x))

obs_by_day17 <- ilbby17f %>%
    mutate(x = 1) %>%
    group_by(observed_on) %>%
    summarize(totobs = sum(x))

obs_by_day18 <- ilbby18f %>%
    mutate(x = 1) %>%
    group_by(observed_on) %>%
    summarize(totobs = sum(x))

obs_by_day19 <- ilbby19f %>%
    mutate(x = 1) %>%
    group_by(observed_on) %>%
    summarize(totobs = sum(x))

obs_by_day20 <- ilbby20f %>%
    mutate(x = 1) %>%
    group_by(observed_on) %>%
    summarize(totobs = sum(x))

# Create calendar heat maps
calendarHeat(obs_by_day16$observed_on, obs_by_day16$totobs, ncolors = 99, 
             color = "lt2dt", 
             varname = "\nFlowering Plant Observations, 2016")

calendarHeat(obs_by_day17$observed_on, obs_by_day17$totobs, ncolors = 99, 
             color = "lt2dt", 
             varname = "\nFlowering Plant Observations, 2017")

calendarHeat(obs_by_day18$observed_on, obs_by_day18$totobs, ncolors = 99, 
             color = "lt2dt", 
             varname = "\nFlowering Plant Observations, 2018")

calendarHeat(obs_by_day19$observed_on, obs_by_day19$totobs, ncolors = 99, 
             color = "lt2dt", 
             varname = "\nFlowering Plant Observations, 2019")

calendarHeat(obs_by_day20$observed_on, obs_by_day20$totobs, ncolors = 99, 
             color = "lt2dt", 
             varname = "\nFlowering Plant Observations, 2020")