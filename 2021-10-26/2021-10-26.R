# Author : Jenn Schilling
# Title: #TidyTuesday Ultra Trail Running
# Date: October 26 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)
library(lubridate)

#### Data #### 

ultra_rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# Frequent racers

freq_racers <- ultra_rankings %>%
  count(runner, gender, sort = TRUE)

# Women-Identifying Racers with More than 20 races
w_freq_racers <- freq_racers %>%
  filter(gender == "W" & n > 20) %>%
  left_join(ultra_rankings, by = c("runner", "gender")) %>%
  left_join(race, by = "race_year_id") %>%
  # Assume races with 0 for the distance are actually 100 miles or 160.93 km
  mutate(distance = ifelse(distance == 0, 160.93, distance)) %>%
  # Remove states from some country entries
  mutate(country = country %>% str_replace("[^,]*, ", "")) %>%
  # Get maximum age since the age doesn't seem to change by year for many runners
  group_by(runner) %>%
  mutate(max_age = max(age)) %>%
  ungroup() %>%
  # Get columns of interest
  select(runner, n, race_year_id, date, rank, max_age, 
         nationality, time_in_seconds, country, distance) %>%
  mutate(race_year = year(date),
         race_month = month(date))

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
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)


#### Plot ####