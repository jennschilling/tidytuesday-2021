# Author : Jenn Schilling
# Title: #TidyTuesday Scooby Doo
# Date: July 13 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Data #### 

scoobydoo <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

scoobydoo_long <- scoobydoo %>%
  select(index, series_name, season, title, date_aired,
         caught_fred, caught_daphnie, caught_velma, caught_shaggy, caught_scooby,
         captured_fred, captured_daphnie, captured_velma, captured_shaggy, captured_scooby,
         unmask_fred, unmask_daphnie, unmask_velma, unmask_shaggy, unmask_scooby) %>%
  pivot_longer(caught_fred:unmask_scooby) %>%
  separate(name, c("type", "character"), "_") %>%
  filter(value == TRUE) %>%
  mutate(character = str_to_title(character),
         type = case_when(
           type == "captured" ~ "Got\nCaptured",
           type == "unmask" ~ "Unmasked\nMonster",
           type == "caught" ~ "Caught\nMonster"
         ) %>%
           factor(levels = c("Got\nCaptured", "Unmasked\nMonster", "Caught\nMonster")))

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
  
  plot.margin = margin(t = 15, r = 50, b = 15, l = 30)
)

# Colors

# Velma Yellow: #F0A200 Maroon: #BF0246
# Shaggy Green: #B2C400 Brown: #A14135
# Scooby Blue: #3EC1BB Brown: #AE7A00 
# Fred Blue: #009AD8 Orange: #EE7C09 (also white)
# Daphnie Purple: #6B4291 Green: #C8D900 Light Purple: #AF94B5


#### Plot ####

ggplot(data = scoobydoo_long,
       mapping = aes(x = as.factor(index),
                     y = type,
                     fill = character)) +
  geom_tile() +
  scale_fill_manual(values = c("#6B4291", # Daphnie
                               "#EE7C09", # Fred
                               "#3EC1BB", # Scooby
                               "#B2C400", # Shaggy
                               "#BF0246" # Velma
                                 )) +
  scale_x_discrete(breaks = c(1, 603),
                   labels = c("Oldest\nEpisode\nSep. 13, 1969", "Newest\nEpisode\nFeb. 25, 2021")) +
  labs(fill = "",
       title = "<b>Scooby Doo:</b> Catching Monsters, Unmasking Monsters, and Getting Captured",
       subtitle = "<br><span style = 'color:#EE7C09;'><b>Fred</b></span> and
       <span style = 'color:#3EC1BB;'><b>Scooby</b></span> caught the most of monsters.
       <span style = 'color:#EE7C09;'><b>Fred</b></span> and 
       <span style = 'color:#BF0246;'><b>Velma</b></span> unmasked the most monsters.
        <span style = 'color:#6B4291;'><b>Daphnie</b></span>, <span style = 'color:#B2C400;'><b>Shaggy</b></span>, 
        and <span style = 'color:#3EC1BB;'><b>Scooby</b></span> got captured the most.<br>",
       caption = "<br><b>Data:</b> Kaggle from plummye | <b>Design:</b> Jenn Schilling") +
  guides(fill = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  theme(axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top")

# Save
ggsave("2021-07-13\\scooby.png",
       plot = last_plot(),
       device = "png",
       width = 11,
       height = 5,
       type = "cairo")
