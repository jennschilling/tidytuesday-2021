# Author : Jenn Schilling
# Title: #TidyTuesday Scooby Doo
# Date: July 13 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Data #### 

scoobydoo <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

#### Formatting ####

font <- "verdana"
title_font <- "Georgia"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor, family = title_font),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = title_font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
)
