# ------------------------------------------------------------------------------
# Waffle calendar
# 
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
    mutate(year = format(observed_on_date, "%Y"),
           month = format(observed_on_date, "%m"),
           monthname = format(observed_on_date, "%B"),
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
waffle.calendar <- trillium %>%
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


ggsave("waffle_calendar.pdf")

# Create calendar heat map, faceted by month (horizontal)

waffle.calendar.faceted <- trillium %>%
    ggplot(aes(x=mo.week.num, y=day, fill=n)) + 
    geom_tile(col="white", width=.9, height=.9) +
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