# Author : Jenn Schilling
# Title: #TidyTuesday Video Games
# Date: Mar 16 2021

#### Libraries ####

library(tidyverse)
library(extrafont)

#### Data #### 

games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 9, color = fontcolor),
  
  legend.title = element_text(size = 12, color = fontcolor),
  legend.text = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 16, face = "bold"),
  
  plot.subtitle = element_text(size = 12, color = fontcolor, margin = margin(b = 15)),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 7, color = fontcolor),
  
  plot.margin = margin(t = 25, r = 25, b = 10, l = 25)
)

#### Exploration ####

ggplot(data = games,
       mapping = aes(x = year,
                     y = avg)) +
  geom_point()
