# Author : Jenn Schilling
# Title: #TidyTuesday Deforestation
# Date: Apr 6 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(patchwork)
library(ggtext)
library(treemap)

#### Data #### 

forest <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
# Note: has data for WORLD

forest_area <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
# Note: has data for many aggregations (continents, regions, etc.; these have NA for code)

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Make Tree Map ####

forest_area_agg <- forest_area %>%
  filter(is.na(code)) %>%
  filter(entity %in% c('Africa', 
                       'Asia', 
                       'Europe', 
                       'Oceania', 
                       'South America', 
                       'Northern America')) %>%
  filter(year %in% c(1990, 2020))

treemap(dtf = forest_area_agg,
        index = c("entity", "year"),
        vSize = "forest_area",
        type = "index")


#### Make Slope Chart ####

ggplot(data = forest_area,
       mapping = aes())