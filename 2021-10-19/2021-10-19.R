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
  )) %>%
  mutate(place_num = parse_number(place),
         weight_lbs = parse_number(weight_lbs),
         ott = parse_number(ott))

# Annotations data
giant_pumpkins <- pumpkins %>%
  filter(type == "Giant Pumpkin") %>%
  filter((year == 2014 & place_num == 1) |
         (year == 2016 & place_num == 1) |
         (year == 2021 & place_num == 1) |
         (year == 2020 & (place_num == 1 | place_num == 2))) %>%
  separate(col = grower_name,
           into = c("last", "first"),
           sep = ", ") %>%
  mutate(grower_name = paste(first, last),
         weight_lbs_lab = number(weight_lbs, accuracy = 1, big.mark = ","),
         country = ifelse(country == "United Kingdom", 
                          "the United Kingdom", country),
         label = paste("In", year, grower_name, "from", country, 
                       "had a pumpkin weighing", weight_lbs_lab, "pounds.")) %>%
  select(year, weight_lbs, label)

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "#FFFFFF"
bcolor <- "#000000"

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

ggplot(data = pumpkins %>%
         filter(type == "Giant Pumpkin"),
       mapping = aes(x = year,
                     y = weight_lbs)) +
  geom_jitter(color = "#FF7518") +
  annotate("text",
           x = giant_pumpkins$year,
           y = giant_pumpkins$weight_lbs,
           label = giant_pumpkins$label,
           color = fontcolor,
           family = font) +
  scale_y_continuous(label = number_format(big.mark = ","),
                     expand = c(0,0)) +
  coord_cartesian(clip = "off") +
  labs(title = "The increasing weight of Giant Pumpkins.",
       subtitle = "",
       x = "",
       y = "",
       caption = "<b>Data:</b> BigPumpkins.com | <b>Design:</b> Jenn Schilling") +
  theme(axis.line = element_blank(),
        axis.ticks.x = element_blank())
