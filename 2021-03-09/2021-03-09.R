# Author : Jenn Schilling
# Title: #TidyTuesday Bechdel Test
# Date: Mar 9 2021

#### Libraries ####

library(tidyverse)
library(extrafont)

#### Data #### 

movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv') %>%
  mutate(domgross_2013 = parse_number(domgross_2013),
         intgross_2013 = parse_number(intgross_2013))

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  axis.title = element_text(size = 9, color = fontcolor),
  axis.text = element_text(size = 8, color = fontcolor),
  
  legend.title = element_text(size = 9, color = fontcolor),
  legend.text = element_text(size = 8, color = fontcolor),
  
  plot.title.position = "plot",
  plot.title = element_text(size = 16, face = "bold"),
  
  plot.subtitle = element_text(size = 9, color = fontcolor, margin = margin(b = 15)),
  
  plot.caption.position = "plot",
  plot.caption = element_text(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 25, r = 25, b = 10, l = 25)
)

#### Exploration ####

# FiveThirtyEight Article:
# https://fivethirtyeight.com/features/the-dollar-and-cents-case-against-hollywoods-exclusion-of-women/

# Recreate FiveThirtyEight plot
movies_pass_fail <- movies %>%
  mutate(year_bin = cut_interval(year, n = 9)) %>%
  group_by(year_bin, clean_test) %>%
  summarise(n = n(),
            .groups = "drop") %>%
  group_by(year_bin) %>%
  mutate(pct = n / sum(n))

ggplot(data = movies_pass_fail,
       mapping = aes(x = year_bin,
                     y = pct,
                     fill = clean_test)) +
  geom_col()


ggplot(data = movies,
       mapping = aes(x = year,
                     y = budget_2013,
                     color = binary)) +
  geom_point() 


movies_pass_fail_yr <- movies %>%
  group_by(year, binary) %>%
  summarise(med_budget = median(budget_2013, na.rm = TRUE),
            med_dom_gross = median(domgross_2013, na.rm = TRUE),
            med_int_gross = median(intgross_2013, na.rm = TRUE),
            med_rating = median(imdb_rating, na.rm = TRUE),
            .groups = "drop")

ggplot(data = movies_pass_fail_yr,
       mapping = aes(x = year,
                     y = med_rating,
                     color = binary)) +
  geom_line()

#### Plot ####



# ggsave("2021-03-09\\.png",
#        plot = last_plot(),
#        device = "png",
#        width = 8,
#        height = 7,
#        type = "cairo")


