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

ggplot(data = summary) +
  geom_segment(mapping = aes(x = premiered,
                             xend = ended,
                             y = viewers_premier,
                             yend = viewers_finale),
               color = fontcolor) +
  geom_point(mapping = aes(x = premiered,
                           y = viewers_premier),
             color = "#48D1CC") +
  geom_point(mapping = aes(x = ended,
                           y = viewers_finale),
             color = "#F77613") +
  scale_y_continuous(labels = number_format(suffix = "M")) +
  labs(subtitle = '<b style="color:#48D1CC">Season Premier</b> <b style="color:#F77613">Season Finale</b>',
       y = "Number of Viewers",
       x = "Show Date",
       caption = "<b>Data:</b> SurvivoR package | <b>Design:</b> Jenn Schilling")

# Add annotations to the plot
# Maybe label points and remove y-axis


