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


bird_baths_types <- bird_baths %>%
  filter(!is.na(survey_year) & bird_count > 0) %>%
  select(survey_year, bioregions, bird_type) %>%
  unique() %>%
  count(survey_year, bioregions) %>%
  pivot_wider(names_from = survey_year,
              values_from = n,
              names_prefix = "yr_") %>%
  mutate(higher_2015 = yr_2015 > yr_2014,
         higher_2015 = ifelse(is.na(higher_2015), FALSE, higher_2015)) %>%
  pivot_longer(yr_2014:yr_2015,
               names_to = "survey_year",
               values_to = "n",
               values_drop_na = TRUE) %>%
  mutate(survey_year = parse_number(survey_year))

bird_baths_labs <- bird_baths_types %>%
  group_by(bioregions) %>%
  summarize(survey_year = max(survey_year),
            .groups = "drop") %>%
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
  plot.title = element_markdown(size = 17, color = fontcolor, family = title_font),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####

ggplot(data = bird_baths_types,
       mapping = aes(x = survey_year,
                     y = n,
                     group = bioregions,
                     color = higher_2015)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_text(data = bird_baths_labs %>% filter(label != "Brigalow Belt South" & 
                                                label != "Flinders Lofty Block" &
                                                label != "Sydney Basin"),
            mapping = aes(label = label),
            family = font,
            lineheight = 0.75,
            fontface = "bold",
            hjust = 0,
            vjust = 0.4,
            nudge_x = 0.01) +
  geom_text(data = bird_baths_labs %>% filter(label == "Brigalow Belt South" | label == "Sydney Basin"),
            mapping = aes(label = label),
            family = font,
            fontface = "bold",
            lineheight = 0.75,
            hjust = 0,
            vjust = -0.1,
            nudge_x = 0.01) +
  geom_text(data = bird_baths_labs %>% filter(label == "Flinders Lofty Block"),
            mapping = aes(label = label),
            family = font,
            fontface = "bold",
            lineheight = 0.75,
            hjust = 0,
            vjust = 1.1,
            nudge_x = 0.01) +
  scale_x_continuous(breaks = c(2014, 2015),
                     limits = c(2014, 2015.2)) +
  scale_y_continuous(limits = c(35, 115),
                     breaks = seq(40, 110, 10)) +
  scale_color_manual(values = c(fontcolor, "#3EC1BB")) +
  coord_cartesian(clip = "off") +
  guides(color = "none") +
  labs(x = "",
       y = "",
       title = "Number of bird species observed during the The Bathing Birds Study in Australia.",
       subtitle = "Two regions saw an <span style = 'color:#3EC1BB;'><b>increase</b></span> in the number of species from 2014 to 2015.",
       caption = "<b>Data:</b> Cleary et al, 2016 | <b>Design:</b> Jenn Schilling") +
  theme(axis.line = element_blank())


# Save
ggsave("2021-08-31\\birds.png",
       plot = last_plot(),
       device = "png",
       width = 10.5,
       height = 9,
       type = "cairo")
