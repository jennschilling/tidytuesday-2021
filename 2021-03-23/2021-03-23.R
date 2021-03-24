# Author : Jenn Schilling
# Title: #TidyTuesday UN Votes
# Date: Mar 23 2021

#### Libraries ####

library(tidyverse)
library(extrafont)

#### Data #### 

library(unvotes)

un_combined <- left_join(un_votes, un_roll_call_issues, by = "rcid") %>%
  left_join(un_roll_calls, by = "rcid") %>%
  mutate(year = lubridate::year(date))

un_combined_summary <- un_combined %>%
  group_by(year, country, issue, vote) %>%
  summarise(n = n(),
            .groups = 'drop') %>%
  ungroup() %>%
  mutate(issue = factor(ifelse(is.na(issue), "Unknown", as.character(issue))))

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  axis.text = element_text(size = 9, color = fontcolor),
  
  legend.title = element_text(size = 12, color = fontcolor),
  legend.text = element_text(size = 10, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 16, color = fontcolor),
  
  plot.subtitle = element_text(size = 12, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 7, color = fontcolor),
  
  plot.margin = margin(t = 25, r = 25, b = 10, l = 25)
)

#### Make Plot ####

