# Author : Jenn Schilling
# Title: #TidyTuesday Star Trek commands
# Date: August 17 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

#### Data #### 

computer <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

computer_plot <- computer %>%
  mutate(type = str_to_sentence(type)) %>%
  count(type, domain, char_type) %>%
  mutate(domain = ifelse(is.na(domain), "Other", domain),
         domain = factor(domain, levels = unique(computer_plot$domain)),
         type = factor(type, levels = unique(computer_plot$type)))

#### Formatting ####

font <- "Candara"
titlefont <- "Candara"
fontcolor <- "#000000"
bcolor <- "#E2D8BF"

star_trek_yellow <- "#EBC41F"
star_trek_blue <- "#01A0D3"
start_trek_red <- "#DC3A51"

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
  plot.title = element_markdown(size = 14, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 12, color = fontcolor, family = font),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)

#### Plot ####

ggplot(mapping = aes(x = domain,
                     y = type,
                     size = n,
                     shape = char_type,
                     color = char_type)) +
  geom_point(data = computer_plot %>% 
                filter(type != "Conversation" | char_type != "Computer")) +
  geom_point(data = computer_plot %>% 
               filter(type == "Conversation" & char_type == "Computer")) +
  scale_y_discrete(limits = rev(levels(computer_plot$type))) +
  scale_x_discrete(position = "top") +
  scale_color_manual(values = c(start_trek_red, star_trek_blue)) +
  scale_shape_manual(values = c(19, 17)) +
  guides(size = guide_legend(override.aes = list(size = c(5, 6, 7)))) +
  labs(x = "",
       y = "") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank())
  