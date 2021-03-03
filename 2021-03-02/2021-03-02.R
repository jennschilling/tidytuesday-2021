# Author : Jenn Schilling
# Title: #TidyTuesday Superbowl Commercials
# Date: Mar 2 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggrepel)

#### Data #### 

youtube <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')


# Fix Hyundai spelling
youtube <- youtube %>%
  mutate(brand = ifelse(brand == "Hynudai", "Hyundai", brand))

#### Formatting ####

font <- "Gill Sans MT"



#### Data Exploration ####

ggplot(data = youtube,
       mapping = aes(x = view_count, 
                     y = like_count,
                     size = comment_count)) +
  geom_point(color = "gray30") +
  facet_wrap(~ brand,
             ncol = 1) +
  scale_x_continuous(labels = scales::label_number_si()) +
  scale_y_continuous(labels = scales::label_number_si()) +
  scale_size_continuous(labels = scales::comma) +
  labs(title = "Superbowl Ads 2000-2020",
       subtitle = "Greater number of views tends to correspond with greater number of likes and comments.",
       x = "Number of Views",
       y = "Number of Likes",
       size = "Number of Comments",
       caption = "Source: FiveThirtyEight | Viz: Jenn Schilling") +
  theme_minimal() +
  theme(text = element_text(family = font),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        
        strip.text = element_text(size = 12,
                                  hjust = 0),
        
        legend.position = "bottom")


