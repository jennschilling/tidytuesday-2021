# Author : Jenn Schilling
# Title: #TidyTuesday Survivor TV Show
# Date: June 1 2021

#### Libraries ####

library(tidyverse)
library(forcats)
library(scales)
library(extrafont)
library(ggtext)

#### Data #### 

summary <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  
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

#### Plot ####

ggplot(data = summary) +
  geom_point(mapping = aes(x = premiered,
                           y = viewers_premier)) +
  geom_point(mapping = aes(x = ended,
                           y = viewers_finale)) +
  geom_segment(mapping = aes(x = premiered,
                             xend = ended,
                             y = viewers_premier,
                             yend = viewers_finale))
