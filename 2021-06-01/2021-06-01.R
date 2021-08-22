# Author : Jenn Schilling
# Title: #TidyTuesday Survivor TV Show
# Date: June 1 2021

#### Libraries ####

library(tidyverse)
library(forcats)
library(scales)
library(extrafont)
library(ggtext)

#### Data #### 

summary <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(color = "white"),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
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

#### Plot ####

# Count how many seasons had higher finale viewership
summary %>%
  mutate(higher_finale = viewers_finale > viewers_premier) %>%
  count(higher_finale) 
# 21 had higher finale viewership, 19 had higher premier viewership

# Make plot
ggplot(data = summary) +
  
  # Line segment and points
  geom_segment(mapping = aes(x = premiered,
                             xend = ended,
                             y = viewers_premier,
                             yend = viewers_finale),
               color = fontcolor,
               size = 0.5) +
  geom_point(mapping = aes(x = premiered,
                           y = viewers_premier,
                           color = "Season Premier"),
             size = 2.5) +
  geom_point(mapping = aes(x = ended,
                           y = viewers_finale,
                           color = "Season Finale"),
             size = 2.5) +
  
  # Scale Definitions
  scale_x_continuous(breaks = c(as_date("2000-01-01"),
                                as_date("2005-01-01"),
                                as_date("2010-01-01"),
                                as_date("2015-01-01"),
                                as_date("2020-01-01")),
                     labels = c("2000", "2005", "2010", "2015", "2020")) +
  scale_y_continuous(labels = number_format(suffix = "M"),
                     position = "right") +
  scale_color_manual(values = c("#FF5E00", "#290FB8")) +
  
  # Annotations
  annotate(geom = "text",
           x = as_date("1995-08-23"),
           y = 13,
           label = "
The 1st season of Survivor premiered in May and 
ended in August 2000. It began with 15.5 million 
viewers and grew to 51.7 million by the finale.",
           family = font,
           color = fontcolor,
           size = 3.5,
           hjust = 0) +
  
  annotate(geom = "text",
           x = as_date("2001-01-28"),
           y = 47,
           label = "
The 2nd season of Survivor premiered in January and 
  ended in May 2001. It began with 45.4 million viewers
  and dropped to 36.4 million by the finale.",
           family = font,
           color = fontcolor,
           size = 3.5,
           hjust = 0) +
  
  annotate(geom = "text",
           x = as_date("2004-02-01"),
           y = 35,
           label = "
The first all-star season with returning players
  sparked a large viewership of 33.5 million for 
  the premier of the 8th season.",
           family = font,
           color = fontcolor,
           size = 3.5,
           hjust = 0) +
  
  annotate(geom = "text",
           x = as_date("2011-02-25"),
           y = 20,
           label = "
Over time, viewership of Survivor season 
premiers and finales generally declined.",
           family = font,
           color = fontcolor,
           size = 3.5,
           hjust = 0) +
  
  # Title
  annotate(geom = "text",
           x = as_date("2019-09-26"),
           y = 55,
           label = "
Survivor season premier and finale viewership over forty seasons
2000-2020",
           family = "Times New Roman",
           color = fontcolor,
           size = 6,
           hjust = 1) +
  
  # Legend
  annotate(geom = "text",
           x = as_date("2016-01-26"),
           y = 48,
           label = "Season Premier",
           family = font,
           color = fontcolor,
           size = 3.5,
           hjust = 0) +
  
  annotate(geom = "point",
           x = as_date("2015-11-26"),
           y = 47.8,
           color = "#290FB8",
           size = 2.5) +
  
  annotate(geom = "text",
           x = as_date("2018-01-26"),
           y = 48,
           label = "Season Finale",
           family = font,
           color = fontcolor,
           size = 3.5,
           hjust = 0) +
  
  annotate(geom = "point",
           x = as_date("2017-11-26"),
           y = 47.8,
           color = "#FF5E00",
           size = 2.5) +
  
  
  
  # Labels and Formatting
  coord_cartesian(clip = "off") +
  guides(color = FALSE) +
  labs(x = "",
       y = "",
       color = "",
       caption = "<b>Data:</b> SurvivoR package | <b>Design:</b> Jenn Schilling")

ggsave("2021-06-01\\survivor.png",
       plot = last_plot(),
       device = "png",
       width = 16,
       height = 8,
       type = "cairo")


