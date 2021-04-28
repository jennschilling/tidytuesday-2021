# Author : Jenn Schilling
# Title: #TidyTuesday CEO Departures
# Date: Apr 27 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(tidytext)

#### Data #### 

departures <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')

departure_reason <- departures %>%
  mutate(notes = gsub('[[:digit:]]+', '', notes)) %>% # remove numbers from notes
  unnest_tokens(word, notes) %>%
  anti_join(stop_words, by = "word") %>% # remove stop words from tokens
  anti_join(tibble(word = tolower(month.name)), by = "word") %>% # remove month names from tokens
  anti_join(tibble(word = tolower(month.abb)), by = "word") 

departure_reason_agg <- departure_reason %>%
  filter(str_detect(string = word, 
                    pattern = "ceo|chairman|executive|president|chief|board|officer|company|corporation|director|corp|c.e.o", 
                    negate = TRUE)) %>%
  group_by(departure_code, word) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  filter(n >= 50)


#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
)