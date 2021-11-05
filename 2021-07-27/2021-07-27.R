# Author : Jenn Schilling
# Title: #TidyTuesday Olympics
# Date: July 27 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Data #### 

olympics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

events_per_person_sport <- olympics %>%
  group_by(name, sex, age, year, season, sport) %>%
  summarise(n_events = n(),
            .groups = "drop") %>%
  ungroup()

#### Formatting ####

font <- "Gill Sans MT"
titlefont <- "Georgia"
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
  plot.title = element_markdown(size = 20, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = font, lineheight = 1.5),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 15, r = 50, b = 15, l = 30)
)

# Olympics Colors
o_blue <- "#0286c3"
o_yellow <- "#fbb22e"
o_black <- "#000000"
o_green <- "#168c39"
o_red <- "#ee2f4d"

#### Plot ####
