################################################################################
# Purpose: Summary final results for ILBBY 2020
# Input data: 
# Output: 
#   -Bar chart showing participation growth across years
#   -Map of observations by county
#   -Number of counties per participant
#   -Map of species by county
# Date: 12/26/2020
################################################################################

library(tidyverse)
library(scales)
library(urbnmapr)

ilbby20 <- read.csv("observations-122044.csv")

# Filter urbanmapr county and state files
states.il <- filter(states, state_name == 'Illinois')
counties.il <- filter(counties, state_name == 'Illinois')

# File below downloaded from https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage 
# on 12/5/2020
counties.pop <- read.csv("co-est2019-alldata.csv")

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

g

################################################################################
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
                      labels = c("0.0","0.1-10.0", "10.1-50.0", "50.1-100.0", "100.1+"))

obs.pop.map

################################################################################
# Test code for mapping coordinate points onto county-level map

latlongpoints <- data.frame(
    lat=c(40.2750747477,
          41.84885298,
          41.8714849523,
          41.9274903043,
          41.9722788202,
          41.98816136,
          41.6416744,
          41.8939371602,
          41.9808581087,
          41.94998667,
          41.7253671313,
          41.747322,
          38.8941311125,
          41.6406745917,
          41.6862401127,
          41.84571777,
          41.84559135,
          41.84519452,
          41.90255772,
          42.0063139686,
          42.0020638889,
          38.6365325865,
          42.00085,
          41.8491642463,
          41.7962322219,
          39.8224363297,
          41.92669746,
          38.8625896583,
          40.1448818716,
          41.92476128,
          41.8603087036,
          41.8603439246,
          40.0683790459,
          41.931066,
          41.9110037713,
          41.8449273069,
          42.00274167,
          40.1083362675,
          41.95838833,
          41.9606334354,
          42.01283511,
          38.3754713655,
          41.8594697,
          41.8598768441,
          40.68540833,
          40.1154289465,
          41.910966991,
          41.9631294654,
          41.9229948858,
          40.708845,
          41.9939833333,
          41.9085436667,
          40.6605582885,
          41.96327167,
          41.9635,
          41.8781136586,
          40.1032679,
          41.989575,
          38.6793511967,
          41.8840658432,
          41.640145,
          41.92387,
          41.9571758361,
          38.6524478008,
          38.80031167,
          40.4876470064,
          41.873453639,
          38.96148,
          41.953879682,
          41.8892837122,
          38.5338888889,
          41.9027672778,
          41.9047477778,
          41.9047477778,
          41.9082949375,
          41.9084622778,
          41.9097966944,
          41.9160228889,
          41.766505,
          41.5573709245,
          41.932739301,
          41.8205624834,
          41.7351350177,
          42.0481450385,
          40.1075764645,
          41.53923031,
          40.4842027,
          42.32934117,
          42.0384,
          42.0331534517,
          41.7634677768,
          39.86375833,
          42.02072677,
          41.8090892789,
          38.5253260212,
          41.7086842843,
          41.92372833,
          41.8999628769,
          41.6928695073,
          41.20935393,
          41.6881253316,
          41.6906684935,
          41.9906019,
          41.7374008,
          41.9011894428,
          42.0516431609,
          41.9886468491,
          41.88574147,
          38.7667788,
          41.8702424192,
          42.0017,
          37.6065880208,
          41.90109667,
          41.9786100493,
          40.4937851322,
          40.4937243538,
          40.4937923038,
          40.489220973,
          40.493594909,
          40.4934696541,
          40.4932198513,
          41.9700536639,
          41.9660118129,
          41.8539335785,
          41.6705160621,
          41.4209319409,
          41.145620414,
          41.9636533209,
          41.9839817661,
          40.705312208,
          41.7178529129,
          41.9646530621,
          41.9730455329,
          40.1094555,
          42.0052456,
          41.9878527778,
          41.2623986539,
          40.1108598388,
          37.6147886856,
          41.9127352127,
          40.12521117,
          41.9484037356,
          38.7677169079,
          38.7651326415,
          41.9315199042,
          41.8549273118,
          41.8235075692,
          41.9871484,
          41.8456779502,
          40.10904717,
          37.71451667,
          41.89623667,
          41.68633596,
          41.7696716014,
          41.67443,
          41.9410195512,
          41.56459874,
          41.6513293,
          40.12736041,
          42.1826736143,
          42.0889011944,
          37.80471167,
          41.4724268695,
          37.7238076731,
          41.862124654,
          40.1139028427,
          40.0651778917),
    lon=c(-90.0411934999,
          -87.6818972,
          -87.6159495564,
          -87.6757725143,
          -87.7132726826,
          -87.73763262,
          -87.5506742,
          -87.8307461648,
          -87.9108911816,
          -88.224005,
          -88.0066549709,
          -87.67812,
          -89.4119867634,
          -87.5505214603,
          -87.5739630684,
          -87.68107696,
          -87.68137914,
          -87.68237325,
          -87.6568792,
          -87.6753348936,
          -87.6862555556,
          -89.8976653442,
          -87.66204167,
          -87.6321729189,
          -87.4177927226,
          -88.9394298157,
          -87.7091303096,
          -90.0541178417,
          -88.1660653186,
          -87.70999137,
          -87.7650537735,
          -87.7650393977,
          -87.9199545244,
          -87.842623,
          -87.6737570018,
          -87.8771805674,
          -87.66384167,
          -88.1990350007,
          -87.95253,
          -87.7049076853,
          -87.8729751,
          -89.6562868852,
          -87.6475129,
          -87.6477689949,
          -89.46141167,
          -88.2583677107,
          -87.6677118528,
          -87.6739637036,
          -87.6855887697,
          -89.62103333,
          -87.6677777778,
          -87.6830779444,
          -89.5587371283,
          -87.67452167,
          -87.67447,
          -87.6297982782,
          -88.2300397,
          -87.667525,
          -90.0110328478,
          -88.0257593841,
          -87.55185833,
          -87.6858583333,
          -87.7194880477,
          -90.0686368404,
          -89.95854167,
          -88.9827370897,
          -87.6258943033,
          -90.35962,
          -87.6125364628,
          -87.7241884206,
          -89.975,
          -87.6797585833,
          -87.6796566944,
          -87.6796566944,
          -87.68043302,
          -87.6823242778,
          -87.6822915833,
          -87.6755825,
          -88.13974717,
          -89.1110491613,
          -87.6797327833,
          -87.7286579416,
          -87.5291881158,
          -88.3011961319,
          -88.2468795882,
          -88.09971676,
          -88.9936873,
          -88.25001582,
          -87.75193,
          -87.6771112913,
          -87.4210590382,
          -88.935005,
          -87.88155004,
          -87.6076718248,
          -89.1219651851,
          -87.8206884955,
          -87.68161167,
          -87.8851880598,
          -87.5693750882,
          -88.00296333,
          -87.5676479593,
          -87.5659328699,
          -87.69463251,
          -87.52948888,
          -87.6787328348,
          -87.8855921491,
          -87.7226120979,
          -88.01398026,
          -89.9802572,
          -87.6623317972,
          -87.7096861111,
          -89.4914407355,
          -87.67596333,
          -87.7405256598,
          -88.9827918211,
          -88.9830085436,
          -88.9829060762,
          -88.9914807184,
          -88.987714272,
          -88.9877561337,
          -88.9826840802,
          -87.7457670401,
          -87.703919951,
          -87.7114043094,
          -87.594191801,
          -88.2285435222,
          -87.8808089713,
          -87.6339394229,
          -87.7064947239,
          -89.6220916135,
          -87.6813481563,
          -87.6746808343,
          -87.7119523287,
          -88.23213333,
          -87.66864273,
          -87.6875305556,
          -88.1713805348,
          -88.2374090881,
          -89.20273917,
          -87.7106021022,
          -88.209045,
          -87.6958197064,
          -89.9783170224,
          -89.9779663236,
          -87.639179472,
          -87.6505227014,
          -87.6771017164,
          -87.6884722,
          -87.758137667,
          -88.218955,
          -89.22908833,
          -87.82192167,
          -87.57409695,
          -87.5662476051,
          -88.039045,
          -87.6944112943,
          -87.64625039,
          -87.6704809,
          -88.21473934,
          -87.7962494777,
          -88.2767989722,
          -89.58200833,
          -90.3373532263,
          -89.2188428889,
          -87.6067443019,
          -88.2378,
          -88.5636841084)
    )

obs.pop.map + geom_point(data=latlongpoints, aes(lon, lat), inherit.aes = FALSE, size=.5)
               

ggsave("ilbby2020obspop.pdf")

