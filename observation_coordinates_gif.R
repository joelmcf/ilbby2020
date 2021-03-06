################################################################################
# Purpose: Observation coordinates gif
# Input data: December 10, 2020 downloads from iNat
# Output: 
#   - gif showing observations plotted over blank county-level map
# Date: December 10, 2020
################################################################################################################################################################

library(tidyverse)
library(lubridate)
library(scales)
library(urbnmapr)
library(gganimate)
library(iNatTools)

# Column names
# observed_on
# latitude
# longitude

# Resources
# -https://cran.r-project.org/web/packages/gganimate/gganimate.pdf

################################################################################
# Filter urbanmapr county and state files
states.il <- filter(states, state_name == 'Illinois')
counties.il <- filter(counties, state_name == 'Illinois')

# Create background map 
map <- ggplot(counties.il, mapping = aes(long, lat, group = group)) + 
    # Add state border
    geom_polygon(data = states.il, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = counties.il, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000", size = 0.05) +
    # Set map projection
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    # Adjust theme elements
    theme(#plot.title = element_text(hjust = 0.5, face="bold"),
          #plot.subtitle = element_text(hjust = 0.5, ),
          panel.background = element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          title = element_text(size=12), 
          plot.subtitle = element_text(size=11),
          plot.caption = element_text(size=12, hjust = 0.60))

################################################################################
# Create animation
# Source example: https://medium.com/business-as-usual-at-solar-analytics/animating-time-series-on-a-map-using-solar-analytic-data-and-r-s-gganimate-package-7831dc3da9d2

trillium <- read.csv("observations-123153.csv")  %>%
    mutate(observed_on_date = ymd(observed_on)) %>%
    mutate(year = format(observed_on_date, "%Y"),
           week = as.integer(format(observed_on_date, "%W")) + 1,
           day = factor(weekdays(observed_on_date, T), 
                        levels = rev(c("Mon", "Tue", "Wed", "Thu", 
                                       "Fri", "Sat", "Sun"))))

typeof(trillium$observed_on) #integer
typeof(trillium$observed_on_date) # double

numdates <- trillium %>%
    distinct(observed_on)

map.trillium <- map +
    geom_point(data=trillium, aes(longitude, latitude), 
               pch=21, fill="#56d800",
               color="black",
               inherit.aes = FALSE, size=3) + 
    transition_states(observed_on_date, transition_length=0) + 
    shadow_mark(fill="#808080", color="#808080", size=1) +
    labs(title = "Prairie Trillium observations", 
         subtitle = expression(italic("(Trillium recurvatum)")),
         caption = "{closest_state}")

map.trillium <- animate(map.trillium, nframes=87, dur=50)

# Export gif
anim_save("trillium.gif", map.trillium)


################################################################################
# Formatting cleanup notes
# Put border around initial geom_point: https://stackoverflow.com/questions/10437442/place-a-border-around-points

# Show static frames for days without data
# Put data in Month XX, XXXX format
# Increase size of title and subtitle
# Add calendar heat map that also builds over time

################################################################################
# Attempt to synch frames to days
#
#trillium.filtered <- trillium %>%
#    filter(observed_on_date <= "2020-06-15")
#
#anim.days <- map +
#    geom_point(data=trillium.filtered, aes(longitude, latitude), 
#               pch=21, fill="#56d800",
#               color="black",
#               inherit.aes = FALSE, size=3) + 
#    transition_time(observed_on_date) + 
#    shadow_mark(fill="#808080", color="#808080", size=1)
#
#anim.days <- animate(anim.days, nframes=87, dur=60)
#
# Export gif
# anim_save("trillium_days.gif", anim.days)
#
################################################################################
claytonia <- read.csv("observations-123415.csv")  %>%
    mutate(observed_on_date = ymd(observed_on))

numdates <- claytonia %>%
    distinct(observed_on_date)
# 78

map.claytonia <- map +
    geom_point(data=claytonia, aes(longitude, latitude), 
               pch=21, fill="#56d800",
               color="black",
               inherit.aes = FALSE, size=3) + 
    transition_states(observed_on_date, transition_length=0) + 
    shadow_mark(fill="#808080", color="#808080", size=1) +
    labs(title = "Virginia Spring Beauty observations", 
         subtitle = expression(italic("(Claytonia virginica)")),
         caption = "{closest_state}")

map.claytonia <- animate(map.claytonia, nframes=78, dur=50)

# Export gif
anim_save("claytonia.gif", map.claytonia)

################################################################################
sanguinaria <- read.csv("observations-123418.csv")  %>%
    mutate(observed_on_date = ymd(observed_on))

numdates <- sanguinaria %>%
    distinct(observed_on_date)
# 89

map.sanguinaria <- map +
    geom_point(data=sanguinaria, aes(longitude, latitude), 
               pch=21, fill="#56d800",
               color="black",
               inherit.aes = FALSE, size=3) + 
    transition_states(observed_on_date, transition_length=0) + 
    shadow_mark(fill="#808080", color="#808080", size=1) +
    labs(title = "Bloodroot observations", 
         subtitle = expression(italic("(Sanguinaria canadensis)")),
         caption = "{closest_state}")

map.sanguinaria <- animate(map.sanguinaria, nframes=89, dur=50)

# Export gif
anim_save("sanguinaria.gif", map.sanguinaria)

################################################################################
# Sanguinaria canadensis data with phenology annotations
sanguinaria.flowering <- iNat(project = "illinois-botanists-big-year-2020", 
                          taxon_id=51044,  
                          term_id = 12, 
                          term_value_id = 13) %>%
    mutate(latitude = as.numeric(gsub(",.*$", "", location)),
           longitude = as.numeric(gsub("^.*,", "", location))) 

sanguinaria.all <- iNat(project = "illinois-botanists-big-year-2020", 
                              taxon_id=51044) %>%
    mutate(latitude = as.numeric(gsub(",.*$", "", location)),
           longitude = as.numeric(gsub("^.*,", "", location)))

flower.ids <- sanguinaria.flowering$id 

sanguinaria.all <- sanguinaria.all %>%
    mutate(indicator = case_when(
        id %in% flower.ids ~ "F", 
        TRUE ~ "V"
    )) 

numdates <- sanguinaria.phen %>%
    distinct(observed_on)
# 38

typeof(sanguinaria.phen$longitude)

map.sanguinaria.all <- map +
    geom_point(data=sanguinaria.all, 
               aes(longitude, latitude,  fill=indicator), 
               pch=21,
               color="black",
               inherit.aes = FALSE, size=3) + 
    transition_states(observed_on, transition_length=0) + 
    shadow_mark(fill="#808080", color="#808080", size=1) +
    labs(title = "Bloodroot observations", 
         subtitle = expression(italic("(Sanguinaria canadensis)")),
         caption = "{closest_state}") +
    scale_fill_manual(values = c("#ffbf00", "#c0ff00"), 
                      labels = c("Flowering", "Non-flowering"), 
                      name = element_blank()) + 
    theme(legend.text = element_text(size = 12), 
          legend.key=element_blank())

map.sanguinaria.all <- animate(map.sanguinaria.all, nframes=78, dur=78)

map.sanguinaria.all

# Export gif
anim_save("sanguinaria.gif", map.sanguinaria.all)
