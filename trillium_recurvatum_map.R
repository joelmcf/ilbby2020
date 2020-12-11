################################################################################
# Trillium recurvatum 

library(tidyverse)
library(scales)
library(urbnmapr)

trillium <- read.csv("observations-123153.csv")

# Column names
# observed_on
# latitude
# longitude

################################################################################
# Create map

# Filter urbanmapr county and state files
states.il <- filter(states, state_name == 'Illinois')
counties.il <- filter(counties, state_name == 'Illinois')

trillium.map <- ggplot(counties.il, mapping = aes(long, lat, group = group)) + 
    # Add state border
    geom_polygon(data = states.il, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = counties.il, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000", size = 0.05) +
    # Set map projection
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    # Add title 
    ggtitle("Trillium recurvatum") +
    # Adjust theme elements
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          panel.background = element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          legend.background = element_rect(fill="gray90")) 

trillium.map + 
    geom_point(data=trillium, aes(longitude, latitude), inherit.aes = FALSE, size=.5)

################################################################################
# Create animation
# Source example: https://medium.com/business-as-usual-at-solar-analytics/animating-time-series-on-a-map-using-solar-analytic-data-and-r-s-gganimate-package-7831dc3da9d2
anim <- trillium.map +
    geom_point(data=trillium, aes(longitude, latitude), inherit.aes = FALSE, size=.5) + 
    transition_states(observed_on, transition_length=0)

anim <- animate(anim)

anim















