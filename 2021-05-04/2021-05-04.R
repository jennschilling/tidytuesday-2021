# Author : Jenn Schilling
# Title: #TidyTuesday Water Point Data Exchange
# Date: May 4 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(ggalluvial)

#### Data #### 

water <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

water_alluvial <- water %>% 
  filter(install_year <= 2021) %>%
  filter(!is.na(water_source)) %>%
  group_by(install_year, water_source) %>%
  summarise(total = n(),
            .groups = "drop") %>%
  group_by(install_year) %>%
  mutate(pct = total / sum(total)) %>%
  select(-total) %>%
  pivot_wider(names_from = "water_source",
              values_from = "pct") %>%
  pivot_longer(-install_year,
               names_to = "water_source",
               values_to = "pct",
               values_drop_na = FALSE) %>%
  mutate(pct = ifelse(is.na(pct), 0 , pct))

#### Formatting ####

font <- "Calibri"
fontcolor <- "gray30"

#### Plot ####

ggplot(data = water %>% filter(install_year <= 2021),
       mapping = aes(y = install_year,
                     fill = water_source)) +
  geom_bar()

ggplot(data = water_alluvial %>% filter(install_year >= 1950),
       mapping = aes(x = install_year,
                     y = pct,
                     fill = water_source)) +
  geom_area(color = "black") 

#### Art ####

ggplot() +
  geom_alluvium(data = water_alluvial %>% filter(install_year >= 1950 & install_year <= 2020),
                mapping = aes(x = install_year,
                              y = pct,
                              alluvium = water_source,
                              fill = water_source),
                alpha = 0.7,
                curve_type = "cubic",
                color = NA) +
  # Colors from https://colorbrewer2.org/#type=sequential&scheme=Blues&n=9
  scale_fill_manual(values = c("#ffffff", "#deebf7", "#c6dbef", "#9ecae1", 
                               "#6baed6", "#4292c6", "#2171b5", "#08519c",
                               "#08306b", "#ffffff", "#ffffff", "#ffffff")) +
  coord_polar(theta = "y",
              clip = "off",
              start = 0,
              direction = -1) +
  guides(fill = FALSE) +
  labs(caption = "**Data:** Water Point Data Exchange | **Design**: Jenn Schilling") +
  theme_void() +
  theme(plot.caption.position = "plot",
        plot.caption = element_markdown(size = 7, color = fontcolor),
        
        plot.margin = margin(t = 15, r = 15, b = 15, l = 15))


ggsave("2021-05-04\\water_art.png",
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 8,
       height = 8,
       dpi = 300)
