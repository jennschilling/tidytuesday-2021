# Author : Jenn Schilling
# Title: #TidyTuesday Water Point Data Exchange
# Date: May 4 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

#### Data #### 

water <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

uganda <- water %>%
  filter(country_name == "Uganda") %>%
  mutate(report_year = parse_number(str_sub(report_date, start = 7, end = 10)))

#### Formatting ####

font <- "Calibri"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.text = element_text(size = 7, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 7, color = fontcolor),
  
  plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
)

#### Plot ####

ggplot(data = uganda %>% filter(!is.na(install_year)),
       mapping = aes(x = install_year,
                     y = report_year,
                     color = water_source)) +
  geom_point() +
  facet_wrap(~ water_source) +
  guides(color = FALSE)
