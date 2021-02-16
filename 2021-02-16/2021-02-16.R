# Author : Jenn Schilling
# Title: #TidyTuesday Du Bois Challenge
# Date: Feb 16 2021

#### Libraries ####

library(tidyverse)


#### Challenge 1 ####

# Data
georgia_pop <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')

georgia_pop_long <- georgia_pop %>%
  pivot_longer(cols = Colored:White,
               names_to = 'race',
               values_to = 'percent') %>%
  rename(year = Year)


ggplot(georgia_pop_long,
       aes(x = percent,
           y = year,
           group = race)) +
  geom_path()

