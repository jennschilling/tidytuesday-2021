# Author : Jenn Schilling
# Title: #TidyTuesday Post Offices
# Date: Apr 13 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(maps)
library(gganimate)
library(ggtext)
library(jsonlite)

#### Data #### 

post_offices <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

# Update Years 
# Some established years are missing 0s at the end
# One discontinued year is missing a leading 1
post_offices <- post_offices %>%
  mutate(established = ifelse(established < 1000, established * 10, established),
         discontinued = ifelse(discontinued < 1000, discontinued + 1000, discontinued))

# Get Arizona Post Offices
post_offices_az <- post_offices %>%
  filter(state == 'AZ') %>%
  pivot_longer(cols = established:discontinued,
               names_to = 'yr_type',
               values_to = 'year',
               values_drop_na = FALSE) 

# Make long data set that has row for each post office and year it's active
max_year <- max(post_offices_az$year, na.rm = TRUE)

post_offices_az_long <- post_offices_az %>%
  mutate(year = ifelse(is.na(year), max_year, year)) %>%
  pivot_wider(names_from = 'yr_type', 
              values_from = 'year') %>%
  mutate(time = discontinued - established + 1) %>%
  uncount(time) %>%
  group_by(id) %>%
  mutate(year = established + seq_along(established) - 1) %>%
  filter(!is.na(latitude))

# Get Arizona Outline
az <- map_data('state') %>%
  filter(region == 'arizona')

# Native Land (https://native-land.ca/)

territories <- fromJSON("https://native-land.ca/coordinates/indigenousTerritories.json")

# Transform JSON to Data Frame
territories_comb <- bind_cols(territories$features$properties, territories$features$geometry)

territories_df <- territories_comb %>%
  unnest(., coordinates) %>%
  unnest(., coordinates) %>%
  separate(coordinates, into = c("long", "lat"), sep = ",") %>%
  mutate(lat = parse_number(lat),
         long = parse_number(long)) %>%
  janitor::clean_names()

# Get Arizona Lands
territories_az_names <- territories_df %>%
  filter(long >= min(az$long) & long <= max(az$long) &
           lat >= min(az$lat) & lat <= max(az$lat)) %>%
  select(name) %>%
  unique(.)

territories_az <- left_join(territories_az_names, territories_df, by = "name") %>%
  mutate(color = ifelse(color == "https://native-land.ca/maps/territories/hia-ced-oodham/", "#2b8cbe", color))

# Tribal Lands Current

tribal_lands <- fromJSON("https://opendata.arcgis.com/datasets/7ae6552b43984219a2791879a83e2baa_27.geojson")

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

# Theme Map from Data Visualization by Kieran Healy (Princeton University Press, 2019)
theme_map <- function(base_size = 9, base_family = font) {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) 
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        panel.spacing=unit(0, "lines"),
        plot.background=element_blank(),
        legend.justification = c(0,0),
        legend.position = c(0,0),
        
        plot.title.position = "plot",
        plot.title = element_markdown(size = 12, color = fontcolor),
        
        plot.subtitle = element_markdown(size = 10, color = fontcolor),
        
        plot.caption.position = "plot",
        plot.caption = element_markdown(size = 8, color = fontcolor),
        
        plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  )
}

#### Make Map ####

p <- ggplot(data = post_offices_az_long,
       mapping = aes(x = longitude,
                     y = latitude)) +
  geom_polygon(data = territories_az,
               mapping = aes(x = long,
                             y = lat,
                             group = name, 
                             fill = color),
               alpha = 0.5) +
  geom_polygon(data = az,
               mapping = aes(x = long,
                             y = lat),
               fill = 'white',
               alpha = 0,
               color = fontcolor,
               size = 1) +
 # guides(fill = FALSE) +
  scale_fill_identity(breaks = territories_az$color,
                      labels = territories_az$name) +
  geom_point(show.legend = FALSE) +
  coord_map() +
  theme_map() +
  scale_color_manual(values = c('white', 'black')) +
  transition_manual(frames = year,
                    cumulative = FALSE) +
  labs(title = 'Post Offices in Arizona',
       subtitle = 'Year: {current_frame}',
       caption = ) +
  theme(legend.position = "right")

# Source: https://stackoverflow.com/questions/56447125/gganimate-not-showing-all-frames
animate(
  plot = p, 
  nframes = length(unique(post_offices_az_long$year)), 
  fps = 4, 
  end_pause = 8
)
