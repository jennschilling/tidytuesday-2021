# Author : Jenn Schilling
# Title: #TidyTuesday Bird Baths
# Date: August 31 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Data #### 

bird_baths <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

bird_baths_total <- bird_baths %>%
  filter(!is.na(survey_year)) %>%
  group_by(survey_year, bioregions) %>%
  summarise(total_birds = sum(bird_count),
            .groups = "drop")

bird_baths_types <- bird_baths %>%
  filter(!is.na(survey_year) & bird_count > 0) %>%
  select(survey_year, bioregions, bird_type) %>%
  unique() %>%
  count(survey_year, bioregions)

bird_baths_labs <- bird_baths_total %>%
  group_by(bioregions) %>%
  summarize(survey_year = min(survey_year),
            .groups = "drop") %>%
  left_join(bird_baths_total, by = c("bioregions", "survey_year")) %>%
  left_join(bird_baths_types, by = c("bioregions", "survey_year")) %>%
  mutate(label = str_wrap(bioregions, width = 30))

#### Formatting ####

font <- "Trebuchet MS"
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
  plot.title = element_markdown(size = 17, color = fontcolor, family = font),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####

ggplot(data = bird_baths_total,
       mapping = aes(x = survey_year,
                     y = total_birds,
                     color = bioregions)) +
  geom_point() +
  geom_line() +
  geom_text(data = bird_baths_labs %>% filter(survey_year == 2015),
            mapping = aes(label = label),
            family = font,
            lineheight = 0.75,
            color = fontcolor,
            hjust = 0,
            vjust = 0) +
  geom_text(data = bird_baths_labs %>% filter(survey_year == 2014),
            mapping = aes(label = label),
            family = font,
            lineheight = 0.75,
            color = fontcolor,
            hjust = 1,
            vjust = 0) +
  scale_x_continuous(breaks = c(2014, 2015),
                     limits = c(2013.5, 2015.5)) +
  scale_y_continuous(labels = comma_format()) +
  coord_cartesian(clip = "off") +
  guides(color = "none") +
  labs(x = "",
       y = "") +
  theme(axis.line = element_blank())


ggplot(data = bird_baths_types,
       mapping = aes(x = survey_year,
                     y = n,
                     color = bioregions)) +
  geom_point() +
  geom_line() +
  geom_text(data = bird_baths_labs %>% filter(survey_year == 2015),
            mapping = aes(label = label),
            family = font,
            lineheight = 0.75,
            color = fontcolor,
            hjust = 0,
            vjust = 0) +
  geom_text(data = bird_baths_labs %>% filter(survey_year == 2014),
            mapping = aes(label = label),
            family = font,
            lineheight = 0.75,
            color = fontcolor,
            hjust = 1,
            vjust = 0) +
  scale_x_continuous(breaks = c(2014, 2015),
                     limits = c(2013.5, 2015.5)) +
  scale_y_continuous(labels = comma_format()) +
  coord_cartesian(clip = "off") +
  guides(color = "none") +
  labs(x = "",
       y = "") +
  theme(axis.line = element_blank())
