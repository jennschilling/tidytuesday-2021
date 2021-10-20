# Author : Jenn Schilling
# Title: #TidyTuesday Giant Pumpkins
# Date: October 19 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Data #### 

pumpkins <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv') %>%
  filter(!str_detect(place, "Entries")) %>%
  separate(col = id,
           into = c("year", "type"), 
           sep = "-") %>%
  mutate(type = case_when(
    type == "F" ~ "Field Pumpkin",
    type == "P" ~ "Giant Pumpkin",
    type == "S" ~ "Giant Squash",
    type == "W" ~ "Giant Watermelon",
    type == "L" ~ "Long Gourd",
    type == "T" ~ "Tomato",
    TRUE ~ "NA"
  )) 

giant_pumpkins <- pumpkins %>%
  filter(type == "Giant Pumpkin") %>%
  filter(place != "EXH" & place != "DMG") %>%
  mutate(place = parse_number(place)) %>%
  filter(place <= 10)

#### Plot ####


