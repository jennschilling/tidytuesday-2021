# Author : Jenn Schilling
# Title: #TidyTuesday Great Lakes Fish
# Date: June 8 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(treemapify)

#### Data #### 

fishing <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(color = "white"),
  
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

#### Aggregate Data ####

total_per_yr_lake <- fishing %>%
  group_by(year, lake) %>%
  summarise(grand_total = sum(grand_total, na.rm = TRUE),
            values_total = sum(values, na.rm = TRUE),
            .groups = "drop") %>%
  filter(year >= 1966)

#### Plot ####

ggplot(data = total_per_yr_lake,
       mapping = aes(area = values_total,
                     fill = lake,
                     color = lake)) +
  geom_treemap() +
  facet_wrap(~year) +
  guides(fill = guide_legend(nrow = 1),
         color = "none") +
  scale_fill_manual(values = c("#233947",
                               "#2F6468",
                               "#455b78",
                               "#09846F",
                               "#6EB7AE",
                               "#52A5C1")) +
  scale_color_manual(values = c("#233947",
                               "#2F6468",
                               "#455b78",
                               "#09846F",
                               "#6EB7AE",
                               "#52A5C1")) +
  labs(title = "Commercial fish production in the Great Lakes, 1966-2015",
       fill = "",
       caption = "<b>Data:</b> Great Lakes Fishery Commission | <b>Design:</b> Jenn Schilling") +
  theme(legend.position = c(0.92, 0.07),
        legend.justification = "right")

ggsave("2021-06-08\\greatlakes.png",
       plot = last_plot(),
       device = "png",
       width = 8,
       height = 8,
       type = "cairo")



