# Author : Jenn Schilling
# Title: #TidyTuesday Netflix
# Date: Apr 20 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(ggalluvial)
library(patchwork)

#### Data #### 

netflix_titles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv') %>%
  mutate(added_year = str_sub(date_added, start = -4)) 

# Aggregate data
netflix_agg_type <- netflix_titles %>%
  filter(!is.na(added_year) & !is.na(type)) %>%
  group_by(added_year, type) %>%
  summarise(num = n(),
            .groups = 'drop') %>%
  group_by(added_year) %>%
  mutate(pct = num / sum(num),
         added_year = parse_number(added_year)) %>%
  filter(added_year <= 2020)

# Labels
type_labels <- tibble(
  x_prop = c(2007.7, 2007.7),
  x_num = c(2020.1, 2020.1),
  y_prop = c(0.25, 0.75),
  y_num = c(697, 1312),
  type = c("TV Show", "Movie"),
  label_prop = c("TV\nShow", "Movie"),
  label_num = c("TV Show", "Movie")
)


#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

netflix_red <- "#db0000"
netflix_grey <- "#564d4d"
black <- "#000000"
white <- "#ffffff"

#### Plot ####

# Sankey Flow Diagram of Proportion
sankey <- ggplot(data = netflix_agg_type,
       mapping = aes(x = added_year,
                     y = pct,
                     stratum = type,
                     alluvium = type,
                     fill = type,
                     label = num)) +
  geom_alluvium() +
  geom_stratum() +
  geom_text(data = type_labels,
            mapping = aes(x = x_prop,
                          y = y_prop,
                          label = label_prop,
                          color = type),
            family = font, 
            size = 4,
            hjust = 1) +
  guides(fill = FALSE,
         color = FALSE) +
  scale_x_continuous(breaks = seq(from = 2008, to = 2020)) +
  scale_fill_manual(values = c(netflix_red, white)) +
  scale_color_manual(values = c(netflix_red, white)) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(title = "Proportion of movies and tv shows added to Netflix each year<br>",
       caption = "Data: <b>Kaggle</b> | Viz: <b>Jenn Schilling</b>") +
  theme_void() +
  theme(axis.text.x = element_text(size = 9, color = white, family = font),
        
        panel.background = element_rect(fill = black),
        plot.background = element_rect(fill = black),
        
        plot.margin = margin(t = 15, r = 45, b = 15, l = 45),
        
        plot.title.position = "plot",
        plot.title = element_markdown(size = 12, color = white, family = font),
        
        plot.caption.position = "plot",
        plot.caption = element_markdown(size = 8, color = white, family = font))

# Line graph of number 
line <- ggplot(data = netflix_agg_type,
       mapping = aes(x = added_year,
                     y = num,
                     group = type,
                     color = type,
                     label = num)) +
  geom_line(size = 1.5) +
  geom_text(data = type_labels,
            mapping = aes(x = x_num,
                          y = y_num,
                          label = label_num,
                          color = type),
            family = font, 
            size = 4,
            hjust = 0) +
  guides(color = FALSE) +
  scale_x_continuous(breaks = seq(from = 2008, to = 2020)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 200),
                     labels = scales::comma) +
  scale_color_manual(values = c(netflix_red, white)) +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(title = "Number of movies and tv shows added to Netflix each year<br>",
       caption = "Data: <b>Kaggle</b> | Viz: <b>Jenn Schilling</b>",
       x = "",
       y = "") +
  theme_bw() +
  theme(axis.text = element_text(size = 9, color = white, family = font),
        axis.line = element_line(color = white),
        axis.ticks = element_line(color = white),
        
        panel.border = element_blank(),
        panel.grid = element_blank(),
        
        panel.background = element_rect(fill = black),
        plot.background = element_rect(fill = black),
        
        plot.margin = margin(t = 15, r = 45, b = 15, l = 15),
        
        plot.title.position = "plot",
        plot.title = element_markdown(size = 12, color = white, family = font),
        
        plot.caption.position = "plot",
        plot.caption = element_markdown(size = 8, color = white, family = font))


# Put plots together
sankey / line +
  theme(panel.background = element_rect(fill = black),
        plot.background = element_rect(fill = black))
