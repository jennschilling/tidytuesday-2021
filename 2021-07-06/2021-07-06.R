# Author : Jenn Schilling
# Title: #TidyTuesday Independence Days
# Date: July 6 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(scales)
library(lubridate)

#### Data #### 

holidays <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-06/holidays.csv') %>%
  filter(!is.na(date_parsed))

# Calendar
cal <- tibble(
  date = seq(ymd(20200101), ymd(20201231), by = 1)
) %>%
  mutate(month = month(date, label = TRUE),
         day = day(date),
         week = as.numeric(format(date, "%U"))) %>%
  left_join(holidays,
            by = c("month", "day")) %>%
  mutate(holiday = ifelse(is.na(date_of_holiday), 0, 1)) %>%
  mutate(weekday = weekdays(date),
         weekday = factor(weekday, c("Sunday", "Monday", "Tuesday",
                                     "Wednesday", "Thursday", "Friday",
                                     "Saturday")),
         month = factor(month, c("Jan", "Feb", "Mar", "Apr",
                                 "May", "Jun", "Jul", "Aug",
                                 "Sep", "Oct", "Nov", "Dec"))) %>%
  group_by(date, month, week, day, weekday) %>%
  summarise(count_holidays = sum(holiday),
            .groups = "drop") 

#### Formatting ####

font <- "Trebuchet MS"
title_font <- "Candara"
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
  plot.title = element_markdown(size = 12, color = fontcolor, family = title_font),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 20, b = 10, l = 20)
)

#### Plot ####

ggplot(data = cal,
       mapping = aes(x = weekday,
                     y = week,
                     fill = count_holidays,
                     label = day)) +
  geom_tile() +
  geom_text() +
  facet_wrap(~ month,
             scales = "free_y") +
  scale_y_reverse()
