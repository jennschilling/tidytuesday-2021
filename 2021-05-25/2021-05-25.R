# Author : Jenn Schilling
# Title: #TidyTuesday Mario Kart 64 World Records
# Date: May 25 2021

#### Libraries ####

library(tidyverse)
library(forcats)
library(extrafont)
library(ggtext)

#### Data #### 

records <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv') %>% 
  filter(!is.na(records))

#### Formatting ####

remotes::install_version("Rttf2pt1", version = "1.3.8") # https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext
library(here)
font_import(path = here("2021-05-25"), pattern = "SuperMario256", prompt = FALSE)
loadfonts(device = "win")

font <- "Super Mario 256"
fontcolor <- "black"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = "white"),
  
  axis.title.x = element_text(size = 10, color = fontcolor),
  axis.text.x = element_text(size = 9, color = fontcolor),
  axis.line.x = element_line(color = fontcolor),
  axis.ticks.x = element_line(color = fontcolor),
  
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank(),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 14, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

# Color Paletter

# Generated from the Tidy Tuesday ReadMe image using https://color.adobe.com/create/image

mario_colors <- c("#A60815", "#193073", "#F2E205", "#F2BD1D", "#F21B1B", # Colorful
                  "#A69296", "#F294C0", "#F2EEB6", "#F2BE22", "#F27E63") # Muted 

### Plot ###

single_lap_records <- records %>% 
  filter(type == "Single Lap") %>%
  mutate(track = fct_rev(track))

single_lap_records_first_last <- single_lap_records %>%
  group_by(track) %>%
  summarise(first = min(date),
            first_time = max(time),
            last = max(date),
            last_time = min(time),
            .groups = "drop")

single_lap_records_remove_first_last <- single_lap_records %>%
  anti_join(single_lap_records_first_last, by = c("track", "date" = "first", "time" = "first_time")) %>%
  anti_join(single_lap_records_first_last, by = c("track", "date" = "last", "time" = "last_time"))

ggplot(data = single_lap_records_remove_first_last,
       mapping = aes(x = date,
                     y = track)) +
  geom_segment(data = single_lap_records_first_last,
               mapping = aes(x = first,
                             xend = last,
                             y = track,
                             yend = track),
               color = fontcolor,
               alpha = 0.4) +
  geom_tile(mapping = aes(height = 0.3),
            color = mario_colors[6]) +
  geom_point(data = single_lap_records_first_last,
             mapping = aes(x = first,
                           y = track),
             color = mario_colors[5],
             size = 2) +
  geom_point(data = single_lap_records_first_last,
             mapping = aes(x = last,
                           y = track),
             color = mario_colors[5],
             size = 2) +
  geom_text(data = single_lap_records_first_last,
            mapping = aes(x = min(first),
                          y = track,
                          label = track),
            family = font,
            color = mario_colors[4],
            vjust = -1,
            hjust = 0) +
  coord_cartesian(clip = "off") +
  labs(title = "Timeline by Track for Single Lap Mario Kart 64 World Records",
       subtitle = "There were a lot of world records in the late 1990s and early 2000s.<br>Since then the pace of new records has slowed.",
       x = "",
       y = "",
       caption = "**Data:** Mario Kart World Records | **Design**: Jenn Schilling")

ggsave("2021-05-25\\mario_kart.png",
       plot = last_plot(),
       device = "png",
       type = "cairo", 
       width = 8,
       height = 10,
       dpi = 300)

