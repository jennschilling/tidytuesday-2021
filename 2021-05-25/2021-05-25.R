# Author : Jenn Schilling
# Title: #TidyTuesday Mario Kart 64 World Records
# Date: May 25 2021

#### Libraries ####

library(tidyverse)
library(forcats)
library(scales)
library(extrafont)
library(ggtext)

#### Data #### 

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text.x = element_text(size = 9, color = fontcolor),
  axis.text.y = element_blank(),
  
  axis.line.x = element_blank(),
  axis.ticks.x = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)