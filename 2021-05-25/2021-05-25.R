# Author : Jenn Schilling
# Title: #TidyTuesday Mario Kart 64 World Records
# Date: May 25 2021

#### Libraries ####

library(tidyverse)
library(forcats)
library(scales)
library(extrafont)
library(ggtext)

#### Data #### 

records <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv') %>% 
  filter(!is.na(records))

#### Formatting ####

# remotes::install_version("Rttf2pt1", version = "1.3.8") # https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext 
# library(here)
# font_import(path = here("2021-05-25"), pattern = "SuperMario256", prompt = FALSE)
# loadfonts(device = "win")

font <- "Super Mario 256"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  axis.line = element_line(color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
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

# Color Paletter

# Generated from the Tidy Tuesday ReadMe image using https://color.adobe.com/create/image

mario_colors <- c("#A60815", "#193073", "#F2E205", "#F2BD1D", "#F21B1B", # Colorful
                  "#A69296", "#F294C0", "#F2EEB6", "#F2BE22", "#F27E63") # Muted 

### Plot ###

single_lap_records <- records %>% filter(type == "Single Lap")

ggplot(data = single_lap_records,
       mapping = aes(x = date,
                     y = time,
                     group = track)) +
  geom_line(color = fontcolor,
            size = 1) +
  facet_wrap(~track,
             scales = "free_y")

set.seed(123)
ggplot(data = single_lap_records,
       mapping = aes(x = record_duration,
                     y = fct_rev(track))) +
  geom_jitter(alpha = 0.5,
              color = fontcolor,
              height = 0.2)
