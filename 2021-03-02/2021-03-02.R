# Author : Jenn Schilling
# Title: #TidyTuesday Super Bowl Commercials
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
  axis.title = element_text(size = 9, color = fontcolor),
  axis.text = element_text(size = 8, color = fontcolor),
  
  legend.title = element_text(size = 9, color = fontcolor),
  legend.text = element_text(size = 8, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 16, face = "bold"),
  
  plot.subtitle = element_text(size = 9, color = fontcolor, margin = margin(b = 15)),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 25, r = 25, b = 10, l = 25)
)

#### Plot ####

ggplot(data = youtube,
       mapping = aes(x = view_count, 
                     y = like_count,
                     size = comment_count)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ brand,
             ncol = 2,
             strip.position = "left",
             scales = "free") +
  scale_x_continuous(limits = c(0, 180000000),
                     breaks = scales::pretty_breaks(),
                     labels = scales::label_number_si()) +
  scale_y_continuous(limits = c(0, 300000),
                     labels = scales::label_number_si()) +
  scale_size_continuous(labels = scales::comma) +
  coord_cartesian(expand = FALSE, 
                  clip = "off") +
  guides(size = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(title = "Super Bowl Ads 2000-2020",
       subtitle = "Number of Views vs. Number of Likes",
       x = "",
       y = "",
       size = "Number of Comments",
       caption = "Source: FiveThirtyEight | Viz: Jenn Schilling") +
  theme(plot.subtitle = element_markdown(size = 10),
        
        plot.caption = element_markdown(),
        
        strip.text.y.left = element_text(size = 10,
                                         angle = 0,
                                         vjust = 1,
                                         hjust = 1,
                                         face = "bold"),
        
        strip.placement = "outside",
        
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing.x = unit(0.5, "cm"),
        
        legend.position = "bottom")

ggsave("2021-03-02\\superbowl_1.png",
       plot = last_plot(),
       device = "png",
       width = 8,
       height = 7,
       type = "cairo")

# Remove outlier of Doritos 2017 Sling Baby

ggplot(data = youtube %>% filter(view_count < 170000000),
       mapping = aes(x = view_count, 
                     y = like_count,
                     size = comment_count)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ brand,
             ncol = 2,
             strip.position = "left",
             scales = "free") +
  scale_x_continuous(limits = c(0, 29000000),
                     breaks = scales::pretty_breaks(),
                     labels = scales::label_number_si()) +
  scale_y_continuous(limits = c(0, 180000),
                     labels = scales::label_number_si()) +
  scale_size_continuous(labels = scales::comma) +
  coord_cartesian(expand = FALSE, 
                  clip = "off") +
  guides(size = guide_legend(title.position = "top",
                             title.hjust = 0.5)) +
  labs(title = "Super Bowl Ads 2000-2020",
       subtitle = "Number of Views vs. Number of Likes",
       x = "",
       y = "",
       size = "Number of Comments",
       caption = "Source: FiveThirtyEight | Viz: Jenn Schilling <br><br>
       Doritos' 2017 *Sling Baby* ad, which had over 170M views, has been excluded") +
  theme(plot.subtitle = element_markdown(),
        
        plot.caption = element_markdown(),
        
        strip.text.y.left = element_text(size = 10,
                                         angle = 0,
                                         vjust = 1,
                                         hjust = 1,
                                         face = "bold"),
        
        strip.placement = "outside",
        
        panel.spacing.y = unit(0.5, "cm"),
        panel.spacing.x = unit(0.5, "cm"),
        
        legend.position = "bottom")

ggsave("2021-03-02\\superbowl_2.png",
       plot = last_plot(),
       device = "png",
       width = 8,
       height = 7,
       type = "cairo")


