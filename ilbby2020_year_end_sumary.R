################################################################################
# Purpose: Summary final results for ILBBY 2020
# Input data: 
# Output: 
#   -Bar chart showing participation growth across years
#   -Map of observations by county
#   -Map of species by county
# Date: 
################################################################################

library(tidyverse)
library(urbnmapr)

ilbby20 <- read.csv("observations-122044.csv")

# List of IL Threatened and Endangered Species
# https://www2.illinois.gov/dnr/ESPB/Documents/ET%20List%20Review%20and%20Revision/Illinois%20Endangered%20and%20Threatened%20Species.pdf

################################################################################
# Bar chart showing participation growth across years

# Create summary dataset taken from ILBBY pages on iNat
finalpart <- data.frame(
    Year = c('2016', '2017', '2018', '2019', '2020'),
    Users = c(420, 1240, 2502, 4287, 5600),
    Species = c(1374, 1512, 1636, 1956, 1988),
    Observations = c(10447, 20972, 40263, 61442, 67633))

# Convert data frame to long format        
finalpart_long <- pivot_longer(finalpart, -Year, names_to = "pvar")

# Reorder variables
finalpart_long2 <- finalpart_long %>%
    mutate(pvar = fct_rev(as_factor(pvar))) 

# Create bar chart
g <- ggplot(finalpart_long2, aes(x = Year, y = value, fill = fct_rev(pvar))) +
    geom_bar(stat="identity", position="dodge") + 
    geom_text(aes(label = comma(value)), 
              position=position_dodge(width=0.9), 
              vjust=-0.25, size=3.5, color = "black") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
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
    scale_y_continuous(expand = c(0, 0), limits = c(0, 75000)) + 
    scale_fill_manual(name = element_blank(), 
                      values = c("Observations" = "#61c9a8", 
                                 "Species" = "#428a73", 
                                 "Users" = "#295648")) +
    ggtitle("ILBBY participation, 2016-2020")

g + theme_classic()

################################################################################
# Map observations by county

# Filter urbanmapr county and state files
states2 <- filter(states, state_name == 'Illinois')
counties2 <- filter(counties, state_name == 'Illinois')

# Create summarized dataset
obs <- ilbby20 %>%
    # Create state name variable for later merge, correct county name errors
    mutate(place_state_name = "Illinois",
           place_county_name = recode(place_county_name, 
                                      "DeWitt" = "De Witt", 
                                      "De Kalb" = "DeKalb",
                                      "La Salle" = "LaSalle")) %>%
    # Append "County" to county variable for later merge
    mutate(place_county_name = paste(place_county_name, "County")) %>% 
    # Summarize number of obs per county
    group_by(place_state_name, place_county_name) %>%
    count(place_county_name)  %>%
    # Join county-level summary table with urbanmapr county shapefile
    full_join(counties, by = c('place_state_name' = 'state_name', 
                               'place_county_name' = 'county_name')) %>% 
    # Filter to include only Illinois counties
    filter(place_state_name == 'Illinois') %>%
    # Replace NA values with 0
    replace_na(list(n = 0)) %>%
    # Create map fill categories
    mutate(obscat = cut(n, c(-1,0,9,99,999,99999)))

obs.summary <- obs %>%
    distinct(place_county_name, n) %>%
    arrange(-n)

write.csv(obs.summary, file = "obs_summary.csv")

# Create map
obs.map <- obs %>%
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
    # Add title 
    ggtitle("Illinois Botanists Big Year 2020") +
    # Adjust theme elements
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
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
    scale_fill_manual(values= c("White", "#64b59b", "#4a9b81", "#3a7965", "#295648"), 
                      guide = "legend", 
                      name = "Research-grade \nplant observations", 
                      labels = c("0","1-9", "10-99", "100-999", "1,000+"))

obs.map

ggsave("ilbby2020obs.pdf")

################################################################################
# Number of counties per participant

counties.part <- ilbby20 %>%
    mutate(place_state_name = "Illinois",
           place_county_name = recode(place_county_name, 
                                      "DeWitt" = "De Witt", 
                                      "De Kalb" = "DeKalb",
                                      "La Salle" = "LaSalle")) %>%
    mutate(place_county_name = paste(place_county_name, "County")) %>% 
    distinct(user_login, place_county_name) %>%
    count(user_login) %>%
    arrange(-n)

write.csv(counties.part, file = "counties_part.csv")


################################################################################
# Number of observations per x residents

# File below downloaded from https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage 
# on 12/5/2020
counties.pop <- read.csv("co-est2019-alldata.csv")

counties.pop.il <- counties.pop %>%
    filter(STNAME == "Illinois" & grepl("County", CTYNAME)) %>%
    select(CTYNAME, POPESTIMATE2019)

counties.obs <- ilbby20 %>%
    # Create state name variable for later merge, correct county name errors
    mutate(place_state_name = "Illinois",
           place_county_name = recode(place_county_name, 
                                      "DeWitt" = "De Witt", 
                                      "De Kalb" = "DeKalb",
                                      "La Salle" = "LaSalle")) %>%
    # Append "County" to county variable for later merge
    mutate(place_county_name = paste(place_county_name, "County")) %>% 
    # Summarize number of obs per county
    group_by(place_state_name, place_county_name) %>%
    count(place_county_name) %>%
    ungroup()

counties.obs.pop <- full_join(counties.obs, counties.pop.il, by = c('place_county_name' = 'CTYNAME')) %>%
    mutate(obs.per.pop = round(n/POPESTIMATE2019*1000, digits=1)) %>%
    arrange(-obs.per.pop) %>%
    replace_na(list(n=0, obs.per.pop=0)) %>%
    rename(county = place_county_name, 
           obs = n,
           pop2019 = POPESTIMATE2019) %>%
    select(county, obs, pop2019, obs.per.pop) %>%
    full_join(counties2, by = c('county' = 'county_name')) %>%
    mutate(obscat = cut(obs.per.pop, c(-1,0,10,50,100,125)))

    

# write.csv(counties.obs.pop, file = "obs_per_pop.csv")

obs.per.pop.map <- counties.obs.pop %>%
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
    # Add title 
    ggtitle("Illinois Botanists Big Year 2020") +
    # Adjust theme elements
    theme(plot.title = element_text(hjust = 0.5, face="bold"),
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
    scale_fill_manual(values= c("White", "#64b59b", "#4a9b81", "#3a7965", "#295648"), 
                      guide = "legend", 
                      name = "Research-grade \nplant observations \nper thousand residents", 
                      labels = c("0","1-10", "11-50", "51-100", "101+"))

obs.per.pop.map

ggsave("ilbby2020obsperpop.pdf")

