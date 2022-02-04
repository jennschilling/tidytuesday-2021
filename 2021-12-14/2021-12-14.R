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
lyrics <- read_csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/lyrics.csv")
related_artists <- read_csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/related_artists.csv")


#### Analysis ####

# Who sings the most and when?

song_word_count <- lyrics %>%
  unnest_tokens(word, line) %>%
  count(album_name, song_name, section_name, section_artist) %>%
  mutate(all = ifelse(str_detect(section_artist, "All") | str_detect(section_artist, "Spice Girls"), n, 0),
         baby = ifelse(str_detect(section_artist, "Baby"), n, 0),
         ginger = ifelse(str_detect(section_artist, "Ginger"), n, 0),
         posh = ifelse(str_detect(section_artist, "Posh") | str_detect(section_artist, "Victoria"), n, 0),
         scary = ifelse(str_detect(section_artist, "Scary"), n, 0),
         sporty = ifelse(str_detect(section_artist, "Sporty"), n, 0)) %>%
  pivot_longer(all:sporty,
               names_to = "artist",
               values_to = "word_count") %>%
  filter(word_count > 0) %>%
  select(-n) %>%
  mutate(section_adj = case_when(
    str_detect(section_name, "Pre-Chorus") ~ "Pre-Chorus",
    TRUE ~ section_name),
    # Most frequent song sections: Bridge, Chorus, Intro, Outro, Pre-Chorus, Verse 1, Verse 2
    freq_section = section_adj %in% c("Bridge", "Chorus", "Intro", "Outro", "Pre-Chorus", "Verse 1", "Verse 2"))


total_song_word_count <- song_word_count %>%
  filter(freq_section) %>%
  group_by(album_name, section_adj, artist) %>%
  summarise(total_words = sum(word_count),
            .groups = "drop") %>%
  ungroup() %>%
  group_by(album_name, section_adj) %>%
  mutate(total_words_section = sum(total_words)) %>%
  ungroup() %>%
  mutate(perc_section_album = total_words / total_words_section,
         section_adj = factor(section_adj,
                              levels = c("Outro",
                                         "Bridge",
                                         "Post-Chorus",
                                         "Chorus",
                                         "Pre-Chorus",
                                         "Verse 2",
                                         "Verse 1",
                                         "Intro")),
         album_name = str_to_upper(album_name),
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
  axis.text.x = element_text(size = 20, family = title_font, color = font_color),
  axis.text.y = element_text(size = 20, color = font_color),
  
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  strip.text = element_text(size = 10, color = font_color, hjust = 0),
  
  legend.text = element_text(size = 10, color = font_color),
  legend.title = element_text(size = 10, color = font_color),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 30, color = font_color, family = title_font),
  
  plot.subtitle = element_markdown(size = 24, color = font_color, family = font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = font_color, hjust = 1),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####

ggplot() +
  geom_text(data = total_song_word_count,
            mapping = aes(x = album_name,
                          y = section_adj,
                          size = perc_section_album,
                          color = artist,
                          label = artist),
            position = position_jitter(width = 0.3, height = 0.3, seed = 8),
            family = font) +
  geom_hline(data = tibble(yintercept = c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5)),
             mapping = aes(yintercept = yintercept)) +
  geom_vline(data = tibble(xintercept = c(1.5, 2.5)),
             mapping = aes(xintercept = xintercept)) +
  scale_color_manual(values = c("#000000", # All
                                "#0079B6", # Baby
                                "#D20A75", # Ginger
                                "#1E7251", # Posh
                                "#EC7E05", # Scary
                                "#CA2D04")) + # Sporty 
  scale_size_continuous(range = c(4, 9)) +
  scale_x_discrete(position = "top") +
  guides(color = "none",
         size = "none") +
  labs(title = "SPICE WORLD")
