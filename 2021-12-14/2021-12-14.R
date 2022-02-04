# Author : Jenn Schilling
# Title: #TidyTuesday Spice Girls
# Date: Dec 14 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(tidytext)

#### Data #### 

studio_album_tracks <- read_csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/studio_album_tracks.csv")
lyrics <- read.csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/lyrics.csv")

#### Analysis ####

# Who sings the most and when?

song_line_count <- lyrics %>%
  count(album_name, song_name, track_number, section_name, section_artist, line_number) %>%
  mutate(all = ifelse(str_detect(section_artist, "All") | str_detect(section_artist, "Spice Girls"), n, 0),
         baby = ifelse(str_detect(section_artist, "Baby"), n, 0),
         ginger = ifelse(str_detect(section_artist, "Ginger"), n, 0),
         posh = ifelse(str_detect(section_artist, "Posh") | str_detect(section_artist, "Victoria"), n, 0),
         scary = ifelse(str_detect(section_artist, "Scary"), n, 0),
         sporty = ifelse(str_detect(section_artist, "Sporty"), n, 0)) %>%
  pivot_longer(all:sporty,
               names_to = "artist",
               values_to = "line_count") %>%
  filter(line_count > 0) %>%
  select(-n) %>%
  mutate(album_name = str_to_upper(album_name),
         album_name = factor(album_name,
                             levels = c("SPICE", "SPICEWORLD", "FOREVER")),
         artist = str_to_title(artist))

#### Formatting ####

font <- "Tw Cen MT Condensed"
title_font <- "Tw Cen MT Condensed Extra Bold"
font_color <- "black"
b_color <- "white"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = b_color, color = NA),
  plot.background = element_rect(fill = b_color, color = NA),
  
  axis.title = element_blank(),
  strip.text = element_text(size = 20, family = title_font, color = font_color),
  strip.text.y.left = element_text(size = 20, family = title_font, color = font_color, angle = 0),
  axis.text = element_blank(),
  
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  legend.text = element_text(size = 10, color = font_color),
  legend.title = element_text(size = 10, color = font_color),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 35, color = font_color, family = title_font),
  
  plot.subtitle = element_markdown(size = 26, color = font_color, family = font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 12, color = font_color, hjust = 1),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####

ggplot() +
  geom_tile(data = song_line_count,
            mapping = aes(y = track_number,
                          x = line_number,
                          fill = artist)) +
  facet_grid(artist ~ album_name,
             switch = "y",
             scales = "free") +
  scale_y_reverse() +
  scale_fill_manual(values = c("#000000", # All
                                "#0079B6", # Baby
                                "#D20A75", # Ginger
                                "#1E7251", # Posh
                                "#EC7E05", # Scary
                                "#CA2D04")) + # Sporty 
  coord_cartesian(clip = "off") +
  guides(fill = "none") +
  labs(title = "SPICE GIRLS | I wanna really, really, really wanna sing a solo",
       subtitle = "<br>Posh is featured the least. Ginger is featured frequently in the first two albums but not on Forever.<br><br>
       <span style = 'font-size:18pt'><b>How to read:</b> Each row is a song in an album. Each column is a line in a song. Songs are in album order. Shaded areas represent who sings the line.<br></span>",
       caption = "<br><b>Data:</b> Spotify and Genius credit Jacquie Tran | <b>Design:</b> Jenn Schilling") 

# Save
ggsave("2021-12-14\\spice_girls.png",
       plot = last_plot(),
       device = "png",
       width = 13,
       height = 12,
       type = "cairo")
