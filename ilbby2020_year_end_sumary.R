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
