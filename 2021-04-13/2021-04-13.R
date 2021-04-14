# Author : Jenn Schilling
# Title: #TidyTuesday Post Offices
# Date: Apr 13 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(patchwork)
library(ggtext)

#### Data #### 

post_offices <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

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
  
  plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
)
