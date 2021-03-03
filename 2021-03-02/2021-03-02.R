# Author : Jenn Schilling
# Title: #TidyTuesday Superbowl Commercials
# Date: Mar 2 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggrepel)
library(ggtext)

#### Data #### 

youtube <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')


# Fix Hyundai spelling
youtube <- youtube %>%
  mutate(brand = ifelse(brand == "Hynudai", "Hyundai", brand))

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  axis.title = element_text(color = fontcolor),
  axis.text = element_text(color = fontcolor),
  
  legend.title = element_text(color = fontcolor),
  legend.text = element_text(color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 16, face = "bold"),
  
  plot.subtitle = element_text(size = 12, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 10, color = fontcolor),
  
  plot.margin = margin(t = 25, r = 25, b = 10, l = 25)
)

#### Plot ####

ggplot(data = youtube,
       mapping = aes(x = view_count, 
                     y = like_count,
                     size = comment_count)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ brand,
             ncol = 1) +
  scale_x_continuous(labels = scales::label_number_si()) +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_size_continuous(labels = scales::comma) +
  coord_cartesian(expand = FALSE, 
                  clip = "off") +
  guides(size = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(title = "Superbowl Ads 2000-2020",
       subtitle = "",
       x = "Number of Views",
       y = "Number of Likes",
       size = "Number of Comments",
       caption = "Source: FiveThirtyEight | Viz: Jenn Schilling") +
  theme(plot.title.position = "plot",
        plot.caption.position = "plot",
        
        strip.text = element_text(size = 12,
                                  hjust = 0),
        
        legend.position = "bottom")


# Remove outlier of Doritos 2017 Sling Baby

ggplot(data = youtube %>% filter(view_count < 170000000),
       mapping = aes(x = view_count, 
                     y = like_count,
                     size = comment_count)) +
  geom_point(color = "gray30") +
  facet_wrap(~ brand,
             ncol = 1) +
  scale_x_continuous(labels = scales::label_number_si()) +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_size_continuous(labels = scales::comma) +
  coord_cartesian(expand = FALSE, 
                  clip = "off") +
  guides(size = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(title = "Superbowl Ads 2000-2020",
       subtitle = "Doritos 2017 *Sling Baby* ad, which had over 170M views, excluded",
       x = "Number of Views",
       y = "Number of Likes",
       size = "Number of Comments",
       caption = "Source: FiveThirtyEight | Viz: Jenn Schilling") +
  theme(plot.subtitle = element_markdown(),
        
        strip.text = element_text(size = 12,
                                  hjust = 0),
        
        legend.position = "bottom")



