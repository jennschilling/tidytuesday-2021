# Author : Jenn Schilling
# Title: #TidyTuesday Public Parks
# Date: June 22 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(ggbump)
library(scales)
library(patchwork)

#### Data #### 

parks <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-22/parks.csv')

parks <- parks %>%
  # Make D.C. same city name
  mutate(city = ifelse(city == "Washington, DC",
                       "Washington, D.C.",
                       city))

tucson <- parks %>%
  filter(city == "Tucson") %>%
  mutate(park_pct_city_data = parse_number(park_pct_city_data) / 100,
         pct_near_park_data = parse_number(pct_near_park_data) / 100,
         spend_per_resident_data = parse_number(spend_per_resident_data))

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

# Color Palette Sources: 
# https://www.color-hex.com/color-palette/37104
# https://i.pinimg.com/originals/71/6e/e6/716ee6bf061d67c8f223ac80a0f1f087.png
tucson_colors <- c("#FF3E7C", #pink
                   "#FFAF84", #orange
                   "#C7BBB5", #grey
                   "#6F62A9", #purple
                   "#CF95B4", #mauve
                   "#152056", #dark blue
                   "#f8a4b1", #pink
                   "#f08125", #golden
                   "#fd7e49", #peach
                   "#c03e34") #rust

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line.x = element_line(color = fontcolor),
  
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

# Create subplots

rank <- ggplot(data = tucson,
       mapping = aes(x = year,
                     y = rank)) +
  geom_bump(size = 1,
            color = tucson_colors[1]) +
  geom_point(size = 2,
             color = tucson_colors[1]) +
  scale_y_reverse() +
  labs(title = "Tucson's rank for local parks among 100 cities in the U.S. has declined since 2012.",
       x = "",
       y = "")

parkarea <- ggplot(data = tucson,
       mapping = aes(x = year,
                     y = park_pct_city_data,
                     group = 1)) +
  geom_line(size = 1,
            color = tucson_colors[4]) +
  geom_point(size = 2,
            color = tucson_colors[4]) +
  scale_y_continuous(limits = c(0.01, 0.04),
                     labels = percent_format(accuracy = 1)) +
  labs(title = "Parkland as percentage of city area in Tucson is quite low.<br>It increased slightly in 2016 and has remained constant since.",
       x = "",
       y = "")

pctresidents <- ggplot(data = tucson,
       mapping = aes(x = year,
                     y = pct_near_park_data,
                     group = 1)) +
  geom_line(size = 1,
            color = tucson_colors[10]) +
  geom_point(size = 2,
            color = tucson_colors[10]) +
  scale_y_continuous(limits = c(0.5, 0.65),
                     labels = percent_format(accuracy = 1)) +
  labs(title = "More than half of Tucson residents live within a 10 minute walk of a park.<br>The percentage of Tucsonans close to parks has increased.",
       x = "",
       y = "")

spend <- ggplot(data = tucson,
       mapping = aes(x = year,
                     y = spend_per_resident_data,
                     group = 1)) +
  geom_line(size = 1,
            color = tucson_colors[9]) +
  geom_point(size = 2,
            color = tucson_colors[9]) +
  scale_y_continuous(limits = c(50, 105),
                     labels = dollar_format(accuracy = 1)) +
  labs(title = "Spending per resident on parks has declined in Tucson since 2013.<br>Spending has been almost cut in half from 2012 to 2020.",
       x = "",
       y = "")

size <- ggplot(data = tucson,
       mapping = aes(x = year,
                     y = med_park_size_data,
                     group = 1)) +
  geom_line(size = 1,
            color = tucson_colors[6]) +
  geom_point(size = 2,
             color = tucson_colors[6]) +
  scale_y_continuous(limits = c(4, 5.5)) +
  labs(title = "Median park size (in acres) in Tucson is quite small.<br>Median size has increased by 1.4 acres from 2012 to 2020.",
       x = "",
       y = "")

# Put subplots together

rank / (spend + size + parkarea + pctresidents) + 
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    title = "<b>There is room for improvement in Tucson's parks.</b>",
    caption = "<b>Data:</b> The Trust for Public Land | <b>Design:</b> Jenn Schilling")

# Save

ggsave("2021-06-22\\tucsonparks.png",
       plot = last_plot(),
       device = "png",
       width = 11,
       height = 11,
       type = "cairo")

