# Author : Jenn Schilling
# Title: #TidyTuesday Star Trek commands
# Date: August 17 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

#### Data #### 

computer <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')


#### Formatting ####

font <- "Candara"
titlefont <- "Candara"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 10, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 12, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 14, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 12, color = fontcolor, family = font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)

