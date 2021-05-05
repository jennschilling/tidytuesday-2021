# Author : Jenn Schilling
# Title: #TidyTuesday Water Point Data Exchange
# Date: May 4 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

#### Data #### 

water <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

uganda <- water %>%
  filter(country_name == "Uganda") %>%
  mutate(report_year = parse_number(str_sub(report_date, start = 7, end = 10)),
         bin_install_year = cut(install_year, breaks = 5, dig.lab = 4),
         bin_report_year = cut(report_year, breaks = 5, dig.lab = 4))

# Latitude and Longitude data does not seem reliable - need to figure out a different viz

uganda_map <- map_data(map = "world") %>%
  filter(region == "Uganda")

uganda_clean <- uganda %>%
  filter(lat_deg <= max(uganda_map$lat) & lat_deg >= min(uganda_map$lat) &
           lon_deg <= max(uganda_map$long) & lon_deg >= min(uganda_map$long))

#### Formatting ####

font <- "Calibri"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.text = element_text(size = 7, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 7, color = fontcolor),
  
  plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
)

#### Plot ####

ggplot(data = uganda %>% filter(!is.na(install_year)),
       mapping = aes(x = install_year,
                     y = report_year,
                     color = water_source)) +
  geom_jitter() +
  facet_wrap(~ water_source) +
  guides(color = FALSE)

ggplot() +
  geom_point(data = uganda_clean,
             mapping = aes(x = lon_deg,
                           y = lat_deg,
                           color = water_source)) +
  geom_polygon(data = uganda_map,
               mapping = aes(x = long,
                             y = lat,
                             group = group),
               fill = NA,
               color = fontcolor) +
  coord_map(projection = "albers",
            lat0 = 1,
            lat1 = 4) +
  facet_wrap(~ bin_install_year)
