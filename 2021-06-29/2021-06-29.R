# Author : Jenn Schilling
# Title: #TidyTuesday Animal Rescues
# Date: June 29 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Data #### 

animal_rescues <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv') %>%
  mutate(incident_notional_cost = parse_number(incident_notional_cost),
         pump_hours_total = parse_number(pump_hours_total),
         animal_group_parent = str_to_title(animal_group_parent)) %>%
  filter(cal_year < 2021)


#### Formatting ####

font <- "Gill Sans MT"
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
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####

top_10_animals <- animal_rescues %>%
  group_by(animal_group_parent) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  top_n(n = 10) %>%
  select(animal_group_parent)

ggplot(data = animal_rescues %>%
         right_join(., top_10_animals, by = "animal_group_parent"),
       mapping = aes(x = incident_notional_cost,
                     y = fct_rev(animal_group_parent),
                     color = animal_group_parent)) +
  geom_boxplot()


ggplot(data = animal_rescues %>%
         right_join(., top_10_animals, by = "animal_group_parent") %>%
         group_by(cal_year, animal_group_parent) %>%
         summarise(n = n(),
                   .groups = "drop"),
       mapping = aes(x = cal_year,
                     y = n,
                     group = animal_group_parent,
                     color = animal_group_parent)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ animal_group_parent,
             ncol = 1) +
  guides(color = FALSE)
