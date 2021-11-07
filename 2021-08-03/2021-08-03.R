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
  count(year, type)

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
  plot.title = element_markdown(size = 20, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = font, lineheight = 1.5),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)


#### Plot ####

ggplot(data = sport_events_year,
       mapping = aes(axis1 = as.factor(year),
                     axis2 = type,
                     y = n)) +
  geom_alluvium(aes(fill = type), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))
  
  annotation_custom(logo,
                    xmin = -5,
                    xmax = 3,
                    ymin = -23) +
  
  
  geom_tile(color = bcolor) +
  scale_fill_gradient(low = "#67B6DB",
                      high = "#014261",
                      na.value = "#F0F0F0",
                      guide = guide_colorbar(title.position = "top",
                                             barwidth = 10,
                                             barheight = 1))  +
  
  coord_cartesian(expand = FALSE,
                  clip = "off") +
  labs(title = "The number of sports and events at the Paralympics have increased over time.<br><br><br><br>",
       x = "",
       y = "",
       fill = "Number of Events",
       caption = "<b>Data:</b> International Paralympic Committee | <b>Logo:</b> International Paralympic Committee | <b>Design:</b> Jenn Schilling") +
  theme(legend.position = "bottom",
        legend.justification = "right",
        axis.line = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12, hjust = 1, vjust = 0),
        axis.text.x = element_text(size = 12))
# Save
ggsave("2021-08-03\\paralympics.png",
       plot = last_plot(),
       device = "png",
       width = 12,
       height = 8,
       type = "cairo")
