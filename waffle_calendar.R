# ------------------------------------------------------------------------------
# Waffle calendar
# Purpose: Create calendar of spring wildflower observations
# Input data: December 2020 downloads from iNat
# Output: Calendar heatmap
# 
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(waffle)
library(padr)

# Prepare dataset
trillium <- read.csv("observations-123153.csv")  %>%
    mutate(observed_on_date = ymd(observed_on)) %>%
    count(observed_on_date) %>%
    pad(start_val = as.Date("2020-01-01"), end_val = as.Date("2020-12-31")) %>%
    mutate(scientific_name = "Trillium recurvatum")

sanguinaria <- read.csv("observations-123418.csv")   %>%
    mutate(observed_on_date = ymd(observed_on)) %>%
    count(observed_on_date) %>%
    pad(start_val = as.Date("2020-01-01"), end_val = as.Date("2020-12-31")) %>%
    mutate(scientific_name = "Sanguinaria canadensis")

claytonia <- read.csv("observations-123415.csv")   %>%
    mutate(observed_on_date = ymd(observed_on)) %>%
    count(observed_on_date) %>%
    pad(start_val = as.Date("2020-01-01"), end_val = as.Date("2020-12-31")) %>%
    mutate(scientific_name = "Claytonia virginica")

combined <- rbind(claytonia, sanguinaria, trillium) %>%
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

# Create basic calendar heat map
waffle.calendar <- combined %>%
    filter(scientific_name == "Trillium recurvatum") %>%
    ggplot(aes(x=week, y=day, fill=n)) + 
    geom_tile(col="white", width=.9, height=.9) +
    scale_fill_viridis_c("", option = "plasma", direction = -1, end = .9) +
    scale_x_continuous(
        expand = c(0,0),
        breaks = seq(1, 52, length = 12), 
        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
        ) + 
    theme_minimal() + 
    theme(panel.grid = element_blank(), 
          legend.position = "bottom", 
          axis.title = element_blank()) + 
    coord_equal()

waffle.calendar


# ggsave("waffle_calendar.pdf")

# Create calendar heat map, faceted by month (horizontal)

day.1l <- c("S", "M", "T", "W", "T", "F", "S")

waffle.calendar.faceted <- combined %>%
    filter(scientific_name == "Trillium recurvatum") %>%
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

waffle.calendar.faceted 

# Create calendar heat map faceted by month (horizontal) and species (vertical)

waffle.calendar.faceted.species <- combined %>%
    ggplot(aes(x=mo.week.num, y=day, fill=n)) + 
    geom_tile(col="white", width=.9, height=.9) +
    scale_y_discrete(labels = rev(day.1l)) + 
    scale_fill_viridis_c("Number of observations", option = "plasma", direction = -1, end = .9) +
    theme_minimal() + 
    theme(title = element_text(face="bold"),
          panel.grid = element_blank(),
          strip.text.y = element_text(face = "italic"),
          legend.position = "bottom", 
          legend.title = element_text(size = 10),
          legend.text = element_text(size=10),
          axis.title = element_blank(), 
          axis.text.x = element_blank()) + 
    coord_equal() + 
    facet_grid(cols = vars(month.abbr), rows = vars(scientific_name),
               labeller = labeller(scientific_name = label_wrap_gen(10))) + 
    labs(title="Illinois Spring Wildflower observations, select species: 2020")

waffle.calendar.faceted.species
