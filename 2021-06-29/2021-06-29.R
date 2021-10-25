# Author : Jenn Schilling
# Title: #TidyTuesday Animal Rescues
# Date: June 29 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)
library(ggimage)

#### Data #### 

animal_rescues <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv') %>%
  mutate(incident_notional_cost = parse_number(incident_notional_cost),
         pump_hours_total = parse_number(pump_hours_total),
         animal_group_parent = str_to_title(animal_group_parent)) %>%
  filter(cal_year < 2021)


# Image Source: http://phylopic.org/image/23cd6aa4-9587-4a2e-8e26-de42885004c9/
# Alternate Image source: https://flyclipart.com/cat-face-png-clipart-head-winging-black-cat-face-clipart-555323#

prop_animal_rescues <- animal_rescues %>%
  count(cal_year, animal_group_parent) %>%
  group_by(cal_year) %>%
  mutate(total = sum(n),
         prop = n / total,
         prop_label = percent(prop, accuracy = 1),
         img =  "2021-06-29\\PhyloPic.23cd6aa4.David-Orr.Felis-silvestris-catus.png" 
         #2021-06-29\\cat-face-png-clipart-head-winging-555323.jpg"
         ) %>%
  ungroup() 

#### Formatting ####

font <- "Trebuchet MS"
title_font <- "Candara"
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
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
)

#### Plot ####

ggplot(data = prop_animal_rescues %>%
         filter(animal_group_parent == "Cat")) +
    geom_image(mapping = aes(x = cal_year,
                             y = cal_year,
                             image = img,
                             color = prop),
               size = 1, 
               asp = 1)+
  scale_size_identity() +
  geom_text(mapping = aes(x = cal_year,
                          y = cal_year,
                          label = prop_label),
            color = "#FFFFFF",
            family = font,
            size = 5,
            hjust = -0.5,
            vjust = 1.5) +
  scale_color_gradient(low = "#67a9cf",
                      high = "#016c59") +
  guides(color = "none") +
  facet_wrap(~ cal_year,
             scales = "free",
             strip.position = "top") +
  labs(title = "Cats account for about half of all animals rescued each year by the London Fire Brigade<br>",
       x = "",
       y = "",
       caption = "<b>Data:</b> London.gov | <b>Image:</b> PhyloPic | <b>Design:</b> Jenn Schilling") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        strip.text = element_text(size = 14, 
                                  color = fontcolor, 
                                  hjust = 0.5),
        plot.title = element_markdown(size = 20, 
                                      color = fontcolor, 
                                      family = title_font, 
                                      margin = margin(0, 0, 20, 0)),
        panel.spacing = unit(2, "lines"))


# Save
ggsave("2021-06-29\\cat_rescues.png",
       plot = last_plot(),
       device = "png",
       width = 12,
       height = 8,
       type = "cairo")
