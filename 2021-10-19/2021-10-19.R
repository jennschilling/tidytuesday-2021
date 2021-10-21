# Author : Jenn Schilling
# Title: #TidyTuesday Giant Pumpkins
# Date: October 19 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)

#### Data #### 

pumpkins <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv') %>%
  filter(!str_detect(place, "Entries")) %>%
  separate(col = id,
           into = c("year", "type"), 
           sep = "-") %>%
  mutate(type = case_when(
    type == "F" ~ "Field Pumpkin",
    type == "P" ~ "Giant Pumpkin",
    type == "S" ~ "Giant Squash",
    type == "W" ~ "Giant Watermelon",
    type == "L" ~ "Long Gourd",
    type == "T" ~ "Tomato",
    TRUE ~ "NA"
  )) %>%
  mutate(place_num = parse_number(place),
         weight_lbs = parse_number(weight_lbs),
         ott = parse_number(ott),
         year = parse_number(year))

# Annotations data
giant_pumpkins <- pumpkins %>%
  filter(type == "Giant Pumpkin") %>%
  filter((year == 2014 & place_num == 1) |
         (year == 2016 & place_num == 1) |
         (year == 2021 & place_num == 1) |
         (year == 2020 & place_num == 1)) %>%
  separate(col = grower_name,
           into = c("last", "first"),
           sep = ", ") %>%
  mutate(grower_name = paste(first, last),
         weight_lbs_lab = number(weight_lbs, accuracy = 0.1, big.mark = ","),
         country = ifelse(country == "United Kingdom", 
                          "the United Kingdom", country),
         label = paste("In", year, grower_name, "from", country, 
                       "grew a pumpkin weighing", weight_lbs_lab, "pounds.") %>%
                 str_wrap(width = 30)) %>%
  select(year, weight_lbs, label)

#### Formatting ####

# Special font
library(showtext)
font_add_google("Griffy")
showtext_auto()
font <- "Griffy"

label_font <- "sans"

fontcolor <- "#FFFFFF"
bcolor <- "#000000"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 30, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 100, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 30, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####


# Base Plot
ggplot(data = pumpkins %>%
         filter(type == "Giant Pumpkin"),
       mapping = aes(x = year,
                     y = weight_lbs)) +
  geom_point(color = "#FF7518",
             position = position_jitter(seed = 42)) +
  scale_y_continuous(label = number_format(big.mark = ","),
                     expand = c(0,0),
                     limits = c(0, 3000),
                     breaks = seq(0, 3000, 500)) +
  scale_x_continuous(breaks = seq(2013, 2021, 1)) +
  coord_cartesian(clip = "off") +
  labs(title = "<b>The Great Giant Pumpkin Weigh-Off.</b>",
       subtitle = "<br>",
       x = "",
       y = "",
       caption = "<b>Data:</b> BigPumpkins.com | <b>Design:</b> Jenn Schilling") +
  theme(axis.line = element_blank(),
        axis.ticks.x = element_blank()) + 
  
  # Annotations
  geom_text(data = giant_pumpkins %>% 
                          filter(year == 2014),
                        mapping = aes(x = year,
                                      y = weight_lbs,
                                      label = label),
                        color = fontcolor,
                        family = label_font,
                        hjust = 0,
                        vjust = 0,
                        position = position_nudge(x = -0.5,
                                                  y = 100),
            size = 10,
            lineheight = 0.3) +
  annotate("curve",
           x = 2013.7, xend = 2014.2,
           y = 2374, yend = 2324,
           curvature = 0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 0.5) +   
  geom_text(data = giant_pumpkins %>% 
              filter(year == 2016),
            mapping = aes(x = year,
                          y = weight_lbs,
                          label = label),
            color = fontcolor,
            family = label_font,
            hjust = 0,
            vjust = 0,
            position = position_nudge(x = 0.2,
                                      y = 100),
            size = 10,
            lineheight = 0.3) +
  annotate("curve",
           x = 2016.5, xend = 2016.27,
           y = 2700, yend = 2625,
           curvature = -0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 0.5)  +   
  geom_text(data = giant_pumpkins %>% 
              filter(year == 2021),
            mapping = aes(x = year,
                          y = weight_lbs,
                          label = label),
            color = fontcolor,
            family = label_font,
            hjust = 0,
            vjust = 0,
            position = position_nudge(x = -1,
                                      y = 200),
            size = 10,
            lineheight = 0.3) +
  annotate("curve",
           x = 2020.8, xend = 2021,
           y = 2855, yend = 2703,
           curvature = 0.2, arrow = arrow(length = unit(2, "mm")),
           color = fontcolor, size = 0.5)  
# Save

ggsave("2021-10-19\\giantpumpkins.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 7,
       type = "cairo")


