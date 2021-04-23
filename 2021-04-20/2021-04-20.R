# Author : Jenn Schilling
# Title: #TidyTuesday Netflix
# Date: Apr 20 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

#### Data #### 

netflix_titles <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv') %>%
  mutate(added_year = str_sub(date_added, start = -4)) 

# Pivot out category labels
netflix_long <- netflix_titles %>%
  mutate(added_year = str_sub(date_added, start = -4)) %>%
  separate(listed_in,
           into = c('cat_1', 'cat_2', 'cat_3'),
           sep = ', ') %>%
  pivot_longer(cols = cat_1:cat_3,
               names_to = 'col',
               values_to = 'category',
               values_drop_na = TRUE) %>%
  select(-col)

# Aggregate data
netflix_agg_type <- netflix_long %>%
  filter(!is.na(added_year) & !is.na(type)) %>%
  group_by(added_year, type) %>%
  summarise(num = n(),
            .groups = 'drop')

netflix_agg_cat <- netflix_long %>%
  filter(!is.na(added_year) & !is.na(category)) %>%
  group_by(added_year, category) %>%
  summarise(num = n(),
            .groups = 'drop')

netflix_agg_rating <- netflix_long %>%
  filter(!is.na(added_year) & !is.na(rating)) %>%
  group_by(added_year, rating) %>%
  summarise(num = n(),
            .groups = 'drop')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
)

#### Plot ####

ggplot(data = netflix_agg_rating,
       mapping = aes(x = added_year,
                     y = num,
                     color = rating,
                     group = rating)) +
  geom_line()

ggplot(data = netflix_titles,
       mapping = aes(x = added_year,
                     group = rating,
                     color = rating)) +
  geom_histogram(stat = "count")

