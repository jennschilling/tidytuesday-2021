# Author : Jenn Schilling
# Title: #TidyTuesday Post Offices
# Date: Apr 13 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(maps)
library(gganimate)

library(ggtext)

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
               values_drop_na = TRUE)

# Remove NA latitude and longitude
post_offices_az_clean <- post_offices_az %>%
  filter(!is.na(longitude) & !is.na(latitude))

# Get Arizona Outline
az <- map_data('state') %>%
  filter(region == 'arizona')

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

p <- ggplot(data = post_offices_az_clean,
       mapping = aes(x = longitude,
                     y = latitude, 
                     color = yr_type)) +
  geom_polygon(data = az,
               mapping = aes(x = long,
                             y = lat),
               fill = 'white',
               color = fontcolor) +
  geom_point(show.legend = FALSE) +
  coord_map() +
  theme_map() +
  scale_color_manual(values = c('white', 'black')) +
  transition_manual(frames = year,
                    cumulative = TRUE) +
  labs(title = 'Post Offices in Arizona',
       subtitle = 'Year: {current_frame}') 

# Source: https://stackoverflow.com/questions/56447125/gganimate-not-showing-all-frames
animate(
  plot = p, 
  nframes = length(unique(post_offices_az_clean$year)), 
  fps = 4, 
  end_pause = 8
)
