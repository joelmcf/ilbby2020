################################################################################
# Purpose: Observation coordinates gif
# Input data: December 10, 2020 downloads from iNat
# Output: 
#   - gif showing observations plotted over blank county-level map
# Date: December 10, 2020
################################################################################################################################################################
# Trillium recurvatum 

library(tidyverse)
library(lubridate)
library(scales)
library(urbnmapr)
library(gganimate)
library(iNatTools)

trillium <- read.csv("observations-123153.csv")  %>%
    mutate(observed_on_date = ymd(observed_on))

typeof(trillium$observed_on) #integer
typeof(trillium$observed_on_date) # double

numdates <- trillium %>%
    distinct(observed_on)

# Column names
# observed_on
# latitude
# longitude

# Resources
# -https://cran.r-project.org/web/packages/gganimate/gganimate.pdf

################################################################################
# Create map

# Filter urbanmapr county and state files
states.il <- filter(states, state_name == 'Illinois')
counties.il <- filter(counties, state_name == 'Illinois')

trillium.map2 <- ggplot(counties.il, mapping = aes(long, lat, group = group)) + 
    # Add state border
    geom_polygon(data = states.il, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = counties.il, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000", size = 0.05) +
    # Set map projection
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    # Add title 
    ggtitle("Trillium recurvatum observations") +
    # Adjust theme elements
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
          plot.subtitle = element_text(hjust = 0.5, ),
          panel.background = element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          legend.background = element_rect(fill="gray90"))

trillium.map1 <- trillium.map2 + 
    labs(subtitle = '{closest_state}')

trillium.map1 + 
    geom_point(data=trillium, aes(longitude, latitude), inherit.aes = FALSE, size=.5)

################################################################################
# Create animation
# Source example: https://medium.com/business-as-usual-at-solar-analytics/animating-time-series-on-a-map-using-solar-analytic-data-and-r-s-gganimate-package-7831dc3da9d2

anim <- trillium.map1 +
    geom_point(data=trillium, aes(longitude, latitude), 
               pch=21, fill="#56d800",
               color="black",
               inherit.aes = FALSE, size=3) + 
    transition_states(observed_on_date, transition_length=0) + 
    shadow_mark(fill="#808080", color="#808080", size=1)

anim <- animate(anim, nframes=87, dur=50)

# Export gif
anim_save("trillium.gif", anim)


################################################################################
# Formatting cleanup notes
# Put border around initial geom_point: https://stackoverflow.com/questions/10437442/place-a-border-around-points

# Show static frames for days without data
# Base initial color on phenology annotations
# Put data in Month XX, XXXX format
# Increase size of title and subtitle
# Add legend
# Add county shading
# Add calendar heat map that also builds over time

# Other species to map
## Claytonia virginica
## Sanguinaria canadensis
## Erythronium albidum

################################################################################
# Attempt to synch frames to days

trillium.filtered <- trillium %>%
    filter(observed_on_date <= "2020-06-15")

anim.days <- trillium.map2 +
    geom_point(data=trillium.filtered, aes(longitude, latitude), 
               pch=21, fill="#56d800",
               color="black",
               inherit.aes = FALSE, size=3) + 
    transition_time(observed_on_date) + 
    shadow_mark(fill="#808080", color="#808080", size=1)


anim.days <- animate(anim.days, nframes=87, dur=60)

anim.days

# Export gif
anim_save("trillium_days.gif", anim.days)

################################################################################
