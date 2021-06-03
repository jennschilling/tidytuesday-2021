# Author : Jenn Schilling
# Title: #TidyTuesday Broadband in the U.S.
# Date: May 11 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(maps)

#### Data #### 

broadband <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-11/broadband.csv')

az_counties <- map_data("county") %>% filter(region == "arizona")

az_broadband <- broadband %>%
  janitor::clean_names() %>%
  filter(st == "AZ") %>%
  mutate(subregion = ifelse(county_name == "La Paz County", "la paz", 
                      ifelse(county_name == "Santa Cruz County", "santa cruz", 
                        tolower(word(county_name)))),
         broadband_usage = parse_number(broadband_usage),
         broadband_availability_per_fcc = parse_number(broadband_availability_per_fcc))

az_broadband_county <- left_join(az_broadband, az_counties, by = "subregion")

#### Formatting ####

font <- "Courier New"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 7, color = fontcolor),
  
  legend.title = element_markdown(size = 8, color = fontcolor),
  legend.text = element_markdown(size = 7, color = fontcolor),
  legend.position = "top",
  legend.direction = "horizontal",
  
  plot.margin = margin(t = 15, r = 0, b = 15, l = 0),
  
)

#### Plot ####

ggplot(data = az_broadband_county,
       mapping = aes(x = long,
                     y = lat,
                     group = group,
                     fill = broadband_usage)) +
  geom_polygon() +
  scale_fill_gradient(low = "#ece2f0",
                      high = "#1c9099",
                      breaks = c(0.15,  0.3, 0.45, 0.6),
                      labels = c("15%", "30%", "45%", "60%")) +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(title = "Percent of people using the internet at<br>25Mbps or above in Arizona",
       fill = "",
       caption = "Viz: <b>Jenn Schilling</b> | Data: <b>The Verge</b>") +
  coord_map() 

ggsave("2021-05-11\\az_broadband.png",
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 5,
       height = 5,
       dpi = 500)
