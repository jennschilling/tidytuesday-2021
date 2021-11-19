# Author : Jenn Schilling
# Title: #TidyTuesday Lemurs
# Date: August 24 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

#### Data #### 

lemurs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

lemur_birth_death <- lemurs %>%
  select(taxon, dlc_id, name, dob, dod, age_at_death_y, age_of_living_y) %>%
  unique() %>%
  arrange(taxon, dob) %>%
  mutate(rnum = row_number()) 

#### Formatting ####

font <- "Consolas"
titlefont <- "Consolas"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 14, color = fontcolor),
  axis.text = element_text(size = 12, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 12, color = fontcolor),
  legend.title = element_text(size = 14, color = fontcolor, lineheight = 0.5),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 16, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 14, color = fontcolor, family = font, lineheight = 1.25),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 10, color = fontcolor, hjust = 1),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)

#### Plot ####

ggplot(data = lemur_birth_death %>% filter(!is.na(dod) & !is.na(dob)),
       mapping = aes(x = dob,
                     xend = dod,
                     y = rnum,
                     yend = rnum)) +
  geom_segment() +
  geom_point() +
  facet_wrap(~ taxon,
             scales = "free") +
  scale_x_date(limits = c(as.Date("1940-01-01"), as.Date("2020-01-01"))) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank())

ggplot(data = lemur_birth_death %>% filter(!is.na(age_at_death_y)),
       mapping = aes(y = taxon,
                     x = age_at_death_y)) +
  geom_point(alpha = 0.5) 
