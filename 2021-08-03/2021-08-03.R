# Author : Jenn Schilling
# Title: #TidyTuesday Paralympics
# Date: August 3 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(ggalluvial)
library(magick)
library(grid)

#### Data #### 

paralympics <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')

sport_events_year <- paralympics %>%
  select(year, type, event) %>%
  unique() %>%
  count(year, type) %>%
  group_by(type) %>%
  mutate(sport_label = ifelse(row_number() == 1, type, NA),
         sport_label_2 = ifelse(year == 2016, type, NA)) %>%
  ungroup()


# Paralympic logo
logo <- image_read("https://www.paralympic.org/sites/default/files/styles/large_original/public/2019-12/IPC%20logo.jpg?itok=lVrcjjSR")
logo <- rasterGrob(logo, interpolate = TRUE)

#### Formatting ####

font <- "Lucida Sans"
titlefont <- "Lucida Sans"
fontcolor <- "#000000"
bcolor <- "#FFFFFF"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 10, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 12, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 16, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = font, lineheight = 1.5),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)


#### Plot ####

ggplot(data = sport_events_year,
       mapping = aes(x = as.factor(year),
                     stratum = type,
                     alluvium = type,
                     fill = type,
                     weight = n,
                     y = n)) +
  geom_alluvium(alpha = 0.8) +
  geom_stratum(width = 1/3, alpha = 0, color = NA) +
  geom_text(data = sport_events_year, #%>%
              #filter(type %in% c("")),
            mapping = aes(label = sport_label,
                          x = as.factor(year)),
            stat = "stratum",
            family = font,
            color = "#FFFFFF",
            size = 4,
            hjust = 0.5,
            nudge_x = 0) +
  guides(fill = "none") +
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  scale_fill_manual(values = c("#208eb7", "#1c4c5e", "#7ec993", "#1c5f1e", 
                               "#9bc732", "#5826a6", "#bcaff9", "#5064be", 
                               "#cd49dc", "#af2168", "#7b3f5b")) +
  labs(title = "The 1984 Paralympics had the greatest number of events and saw the addition of Powerlifting. Since then, the number<br>
  <br>of overall events has declined. Rugby was added to the 1996 games, and  the Triathlon was added in 2016.<br><br>",
       x = "",
       y = "",
       caption = "<b>Data:</b> International Paralympic Committee | <b>Logo:</b> International Paralympic Committee | <b>Design:</b> Jenn Schilling") +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10))
  
  

# Save
ggsave("2021-08-03\\paralympics.png",
       plot = last_plot(),
       device = "png",
       width = 14,
       height = 9,
       type = "cairo")
