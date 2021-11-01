# Author : Jenn Schilling
# Title: #TidyTuesday US Drought
# Date: July 20 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)
library(lubridate)
library(geofacet)

#### Data #### 

drought <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

# Put data into a format with months and week numbers
# Note have data starting in half of July 2001 through half of July 2021
# Aggregate drought levels
drought_d <- drought %>%
  mutate(year_start = year(valid_start),
         month_start = month(valid_start),
         week_num_start = week(valid_start)) %>%
  filter(stat_fmt == 2) %>%
  select(-area_total, -pop_pct, -pop_total) %>%
  pivot_wider(names_from = "drought_lvl",
              values_from = "area_pct") %>%
  mutate(drought = D0 + D1 + D2 + D3 + D4,
         no_drought = None) %>%
  select(state_abb, valid_start, year_start, month_start, week_num_start, drought, no_drought) %>%
  pivot_longer(drought:no_drought,
               names_to = "drought",
               values_to = "area_pct") %>%
  mutate(drought = factor(drought, levels = c("no_drought", "drought")))

#### Formatting ####

font <- "Trebuchet MS"
titlefont <- "Georgia"
fontcolor <- "gray30"
bcolor <- "#F8F8F8"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 10, color = fontcolor),
  legend.title = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 20, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 11, color = fontcolor, family = font, lineheight = 1.5),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor, hjust = 1.04),
  
  plot.margin = margin(t = 15, r = 50, b = 15, l = 30)
)

#### Plot ####

ggplot(data = drought_d,
       mapping = aes(x = valid_start,
                     y = area_pct,
                     fill = drought)) +
  geom_area() +
  scale_fill_manual(values = c(bcolor, "#EE7C09"),
                    labels = c("No Drought", "Drought")) +
  guides(fill = "none") +
  facet_geo(~ state_abb) +
  labs(title = "Many states are consistently in some level of drought; western states experience the most drought.",
       subtitle = "<br>Graphs show the percent of the land area in each state that is in <span style = 'color:#EE7C09;'><b>drought</b></span> every week from July 2001 to July 2021.
       <br>Drought includes every level of drought classification from U.S. Drought Monitor from abnormally dry to exceptional drought.<br>",
       x = "",
       y = "",
       caption = "<br><b>Data:</b> U.S. Drought Monitor | <b>Design:</b> Jenn Schilling") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Save
ggsave("2021-07-20\\usdrought.png",
       plot = last_plot(),
       device = "png",
       width = 14,
       height = 8,
       type = "cairo")
