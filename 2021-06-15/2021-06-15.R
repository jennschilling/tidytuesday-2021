# Author : Jenn Schilling
# Title: #TidyTuesday #DuBoisChallenge tweets
# Date: June 15 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(lubridate)

#### Data #### 

tweets <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-15/tweets.csv')

#### Format ####

title_font <- "Consolas"
axis_font <- "Microsoft Sans Serif" 

background <- "#e5d4c3"
grid_line <- "#dab198"
fontcolor <- "gray40"

theme_set(theme_minimal(base_size = 12, base_family = axis_font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color = grid_line),
  
  plot.background = element_rect(fill =  background, color = NA),
  panel.background = element_rect(fill = background, color = NA),
  panel.border = element_rect(fill = NA, color = fontcolor),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 14, color = "black", 
                                face = "bold", family = title_font,
                                hjust = 0.5),
  
  plot.subtitle = element_markdown(size = 10, family = title_font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8),
  
  plot.margin = margin(t = 10, r = 25, b = 10, l = 25)
)

#### Aggregate ####

agg_tweets <- tweets %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  filter(!is.na(date))

#### Plot ####

ggplot(data = agg_tweets,
       mapping = aes(x = date,
                     y = n,
                     group = 1)) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0, 80),
                     breaks = seq(0, 80, 10)) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  coord_cartesian(expand = FALSE) +
  labs(title = "#DUBOISCHALLENGE TWEETS BY DAY",
       subtitle = "",
       x = "",
       y = "",
       caption = "<b>Data:</b> #DuBoisChallenge Tweets | <b>Design:</b> Jenn Schilling")

ggsave("2021-06-15\\duboischallenge.png",
       plot = last_plot(),
       device = "png",
       width = 7,
       height = 5,
       type = "cairo")
