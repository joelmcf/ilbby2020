# ------------------------------------------------------------------------------
# Purpose: Summary final results for ILBBY 2020
# Input data: 
# Output: 
#   -Bar chart showing participation growth across years
#   -Map of observations by county
#   -Number of counties per participant
#   -Map of species by county
#   -Calendar heat map of observations
# Date: 12/26/2020
# ------------------------------------------------------------------------------

library(tidyverse)
library(scales)
library(urbnmapr)
library(waffle)
library(padr)

ilbby20 <- read.csv("observations-122044.csv")

# Filter urbanmapr county and state files
states.il <- filter(states, state_name == 'Illinois')
counties.il <- filter(counties, state_name == 'Illinois')

# File below downloaded from https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage 
# on 12/5/2020
counties.pop <- read.csv("co-est2019-alldata.csv")

# List of IL Threatened and Endangered Species
# https://www2.illinois.gov/dnr/ESPB/Documents/ET%20List%20Review%20and%20Revision/Illinois%20Endangered%20and%20Threatened%20Species.pdf

# ------------------------------------------------------------------------------
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
          axis.text.x=element_text(size = 10, color = "black"),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank(),
          legend.position = 'bottom', 
          legend.text = element_text(size = 10, color = "Black"),
          plot.title = element_text(face = "bold", hjust = 0.5)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 75000)) + 
    scale_fill_manual(name = element_blank(), 
                      values = c("Observations" = "#61c9a8", 
                                 "Species" = "#428a73", 
                                 "Users" = "#295648")) +
    ggtitle("ILBBY participation, 2016-2020")

g

# ------------------------------------------------------------------------------
# Observations by county

# Create summarized dataset for mapping
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
    full_join(counties.il, by = c( 'place_county_name' = 'county_name')) %>% 
    # Replace NA values with 0
    replace_na(list(n = 0)) %>%
    # Create map fill categories
    mutate(obscat = cut(n, c(-1,0,9,99,999,99999)))

# Subset dataset for export to CSV
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
    geom_polygon(data = states.il, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = counties.il, mapping = aes(long, lat, group = group),
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
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          legend.background = element_rect(fill="gray90")) +
    # Select color palette, format legend
    scale_fill_manual(values= c("White", "#64b59b", "#4a9b81", "#3a7965", "#295648"), 
                      guide = "legend", 
                      name = "Research-grade \nplant observations", 
                      labels = c("0","1-9", "10-99", "100-999", "1,000+"))

obs.map

ggsave("ilbby2020obs.pdf")

# ------------------------------------------------------------------------------
# Counties per participant

# Create summarized dataset for export to CSV
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

# write.csv(counties.part, file = "counties_part.csv")


################################################################################
# Number of observations per 1,000 residents

counties.pop.il <- counties.pop %>%
    filter(STNAME == "Illinois" & grepl("County", CTYNAME)) %>%
    select(CTYNAME, POPESTIMATE2019)

# Create summarized dataset for mapping
obs.pop <- ilbby20 %>%
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
    ungroup() %>%
    full_join(counties.pop.il, by = c('place_county_name' = 'CTYNAME')) %>%
    mutate(obs.per.pop = round(n/POPESTIMATE2019*1000, digits=1)) %>%
    arrange(-obs.per.pop) %>%
    replace_na(list(n=0, obs.per.pop=0)) %>%
    rename(county = place_county_name, 
           obs = n,
           pop2019 = POPESTIMATE2019) %>%
    select(county, obs, pop2019, obs.per.pop) %>%
    full_join(counties.il, by = c('county' = 'county_name')) %>%
    mutate(obscat = cut(obs.per.pop, c(-1,0,10,50,100,125))) %>%
    ungroup

# Subset data for export to CSV
obs.pop.summary <- obs.pop %>%
    distinct(county, obs, pop2019, obs.per.pop) %>%
    arrange(-obs.per.pop)

#write.csv(obs.pop.summary, file = "obs_pop_summary.csv")

# Create map
obs.pop.map <- obs.pop %>%
    # Fill counties based on number of observations
    ggplot(mapping = aes(long, lat, group = group, fill = obscat)) + 
    geom_polygon(color = NA) +
    # Add state border
    geom_polygon(data = states.il, mapping = aes(long, lat, group = group),
                 fill = NA, color = "#000000") +
    # Add county borders
    geom_polygon(data = counties.il, mapping = aes(long, lat, group = group),
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
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          legend.background = element_rect(fill="gray90")) +
    # Select color palette, format legend
    scale_fill_manual(values= c("White", "#64b59b", "#4a9b81", "#3a7965", "#295648"), 
                      guide = "legend", 
                      name = "Research-grade \nplant observations \nper thousand residents", 
                      labels = c("0.0","0.1-10.0", "10.1-50.0", "50.1-100.0", "100.1+"))

obs.pop.map

ggsave("ilbby2020obspop.pdf")

# ------------------------------------------------------------------------------
# Observation calendar heat map

calendar <- ilbby20 %>%
    mutate(observed_on_date = ymd(observed_on)) %>%
    count(observed_on_date) %>%
    pad(start_val = as.Date("2020-01-01"), end_val = as.Date("2020-12-31")) %>%
    mutate(month = format(observed_on_date, "%m"),
           week = as.integer(format(observed_on_date, "%W")) + 1,
           day = factor(weekdays(observed_on_date, T), 
                        levels = rev(c("Sun", "Mon", "Tue", "Wed", "Thu", 
                                       "Fri", "Sat"))),
           mo.week.num= epiweek(observed_on_date) - epiweek(floor_date(observed_on_date, unit = "month"))+1) %>%
    mutate(month.abbr = factor(case_when(
        month == "01" ~ "Jan", 
        month == "02" ~ "Feb", 
        month == "03" ~ "Mar", 
        month == "04" ~ "Apr", 
        month == "05" ~ "May", 
        month == "06" ~ "Jun",
        month == "07" ~ "Jul", 
        month == "08" ~ "Aug", 
        month == "09" ~ "Sep", 
        month == "10" ~ "Oct", 
        month == "11" ~ "Nov", 
        month == "12" ~ "Dec"
    ), 
    levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))   


day.1l <- c("S", "M", "T", "W", "T", "F", "S")

waffle.calendar <- calendar %>%
    ggplot(aes(x=mo.week.num, y=day, fill=n)) + 
    geom_tile(col="white", width=.9, height=.9) +
    scale_y_discrete(labels = rev(day.1l)) + 
    scale_fill_viridis_c("", option = "plasma", direction = -1, end = .9) +
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          panel.spacing.x = unit(.001, "lines"),
          legend.position = "bottom", 
          axis.title = element_blank(), 
          axis.text.x = element_blank()) + 
    coord_equal() + 
    facet_grid(cols = vars(month.abbr))

waffle.calendar