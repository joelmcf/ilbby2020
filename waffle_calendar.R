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
           week = as.integer(format(observed_on_date, "%W")) + 1,
           day = factor(weekdays(observed_on_date, T), 
                        levels = rev(c("Mon", "Tue", "Wed", "Thu", 
                                       "Fri", "Sat", "Sun"))))

# Create calendar heat map
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