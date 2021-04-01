# Author : Jenn Schilling
# Title: #TidyTuesday Makeup Shades
# Date: Mar 30 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(patchwork)
library(ggtext)

#### Data #### 

allCategories <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')
allShades <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####

all_shades <- allShades %>%
  mutate(lightness_group = cut_interval(lightness, 20)) %>%
  group_by(lightness_group) %>%
  arrange(sat, .by_group = TRUE) %>%
  mutate(y = row_number()) %>%
  ungroup()

ggplot(data = all_shades,
       mapping = aes(x = lightness_group,
                     y = y,
                     fill = hex)) +
  geom_tile() +
  scale_fill_identity() +
  guides(fill = FALSE) +
  labs(title = "Distribution of 6,816 foundation shades from Ulta and Sephora",
       x = "",
       y = "") +
  coord_cartesian(expand = FALSE) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())


nude_shades <- allShades %>%
  filter(name %in% c("Nude", "Neutral")) %>%
  mutate(lightness_group = cut_interval(lightness, 20)) %>%
  group_by(lightness_group) %>%
  arrange(sat, .by_group = TRUE) %>%
  mutate(y = row_number()) %>%
  ungroup()

ggplot(data = nude_shades,
       mapping = aes(x = lightness_group,
                     y = y,
                     fill = hex)) +
  geom_tile(color = 'white') +
  scale_fill_identity() +
  guides(fill = FALSE) +
  labs(title = "Distribution of 176 **Nude/Neutral** foundation shades from Ulta and Sephora",
       x = "",
       y = "") +
  coord_cartesian(expand = FALSE) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())


