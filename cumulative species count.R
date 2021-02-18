library(tidyverse)
library(lubridate)
library(scales)
library(padr)

ilbby20 <- read.csv("observations-2020.csv")
ilbby19 <- read.csv("observations-2019.csv")
ilbby18 <- read.csv("observations-2018.csv")
ilbby17 <- read.csv("observations-2017.csv")
ilbby16 <- read.csv("observations-2016.csv")
earlier <- read.csv("observations-earlier.csv")

ilbby <- rbind(earlier, ilbby16, ilbby17, ilbby18, ilbby19, ilbby20)

cumulative_species <- ilbby %>%
    select(taxon_species_name, observed_on) %>%
    filter(taxon_species_name != "") %>%
    arrange(taxon_species_name, observed_on) %>%
    mutate(observed_on = ymd(observed_on)) %>%
    mutate(moyr = floor_date(observed_on, "month")) %>%
    group_by(taxon_species_name) %>%
    mutate(id = row_number()) %>%
    filter(id == 1) %>%
    ungroup() %>%
    group_by(moyr) %>%
    count(id) %>%
    ungroup() %>%
    arrange(moyr) %>%
    mutate(cumsum = cumsum(n)) %>%
    mutate(carryover = cumsum-n) %>%
    rename(a = n, b = carryover) %>%
    select(moyr, a, b) %>%
    filter(moyr >= "2016-1-1") %>%
    pad() %>%
    mutate(xvar = row_number()) %>%
    pivot_longer(cols = c("a", "b")) %>%
    arrange(moyr) %>%
    mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%
    mutate(value = case_when(
        xvar == 12 & name == "b" ~ 1553,
        xvar == 26 & name == "b" ~ 1771, 
        TRUE ~ value
    ))


cumulative_plot <- ggplot(cumulative_species) +
    geom_bar(aes(x = xvar, y = value, fill = name), stat = "identity", position = "stack", width = 1) +
    scale_fill_manual(labels = c("New species", "Running total"), 
                      values = c("green", "#a4a4a4")) +
    theme(legend.title = element_blank(), 
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.ticks.y = element_line(color = "black", size = .2),
          axis.text.x = element_text(hjust = .25) ) +
    labs(y = "Species")  + 
    scale_y_continuous(limits = c(0, 3000),
                       breaks = seq(0, 3000, 500),
                       expand = c(0, 0), 
                       label = comma) +
    scale_x_continuous(limits = c(0, 61),
                       breaks = seq(1, 60, 12), 
                       labels = paste0(c("2016", "2017", "2018", "2019", "2020")),
                       expand = c(0, 0)) +  
    geom_vline(xintercept = 0.5, color = "black", size = .2) + 
    geom_vline(xintercept = 12.5, color = "black", size = .2) + 
    geom_vline(xintercept = 24.5, color = "black", size = .2) + 
    geom_vline(xintercept = 36.5, color = "black", size = .2) + 
    geom_vline(xintercept = 48.5, color = "black", size = .2)

cumulative_plot

ggsave("cumulative_plot.png")

# New species in 2020
new_species_2020 <- ilbby %>%
    select(taxon_species_name, observed_on) %>%
    filter(taxon_species_name != "") %>%
    arrange(taxon_species_name, observed_on) %>%
    mutate(observed_on = ymd(observed_on)) %>%
    mutate(moyr = floor_date(observed_on, "month")) %>%
    group_by(taxon_species_name) %>%
    mutate(id = row_number()) %>%
    filter(id == 1) %>%
    filter(moyr >= "2020-1-1")

write.csv(new_species_2020, "new_species_2020.csv")
