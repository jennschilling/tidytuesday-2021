# Author : Jenn Schilling
# Title: #TidyTuesday Deforestation
# Date: Apr 6 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(patchwork)
library(ggtext)
library(treemap)
library(ggbump)

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
  
  plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
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


#### Make Bump Chart ####

forest_area_agg <- forest_area %>%
  filter(is.na(code)) %>%
  filter(entity %in% c('Africa', 
                       'Asia', 
                       'Europe', 
                       'Oceania', 
                       'South America', 
                       'Northern America')) %>%
  filter(year %in% c(1990, 2000, 2010, 2020))

ggplot(data = forest_area_agg,
       mapping = aes(x = year,
                     y = forest_area,
                     color = entity)) +
  geom_point(size = 5) +
  geom_bump(size = 2, 
            smooth = 50) +
  geom_text(data = forest_area_agg %>% filter(year == 2020),
            mapping = aes(x = year + 0.5,
                          y = forest_area,
                          color = entity,
                          label = entity),
            hjust = 0,
            size = 5,
            family = font) +
  guides(color = FALSE) +
  scale_x_continuous(limits = c(1990, 2024.5)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1)) +
  scale_color_manual(values = c("#addd8e", "#78c679", "#41ab5d",
                                "#238443", "#006837", "#004529")) +
  coord_cartesian(clip = "off") +
  labs(x = "",
       y = "",
       title = "Percent of global forest area by continent, 1990-2020",
       caption = "Data: <b>Our World In Data</b> | Viz: <b>Jenn Schilling</b>") +
  theme(panel.grid = element_blank(),
        
        axis.text = element_text(size = 12, color = fontcolor),
        plot.title = element_markdown(size = 18, color = fontcolor),
        plot.caption = element_markdown(size = 10, color = fontcolor),
        
        axis.ticks.y = element_line(size = 0.7, color = fontcolor))

ggsave("2021-04-06\\percent_forest.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 10,
       dpi = 300,
       type = "cairo")
