# Author : Jenn Schilling
# Title: #TidyTuesday Water Point Data Exchange
# Date: May 4 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(ggalluvial)

#### Data #### 

water <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv')

# Add a random parameter
set.seed(5621)
rnd <- tibble(rnd = round(runif(n = nrow(water), min = 1, max = 5)))
water_rnd <- bind_cols(water, rnd) %>% 
  filter(!is.na(install_year) & !is.na(water_source) &
           lat_deg >= -25 & lat_deg <= 25 &
           lon_deg >= -50 & lon_deg <= 50) 

#### Formatting ####

font <- "Calibri"
fontcolor <- "gray30"

#### Art ####

ggplot(data = water %>% sample_n(100000),
       mapping = aes(x = lat_deg,
                     xend = lon_deg,
                     y = lon_deg,
                     yend = -lat_deg,
                     color = water_source)) +
  geom_segment() +
 # coord_polar() +
  guides(color = FALSE)




ggplot(data = water_rnd,
       mapping = aes(x = lat_deg,
                     xend = lon_deg,
                     y = lon_deg,
                     yend = lat_deg,
                     #shape = water_source,
                     #color = install_year,
                     color = water_source,
                     size = rnd)) +
  geom_segment(alpha = 0.7) +
  # scale_shape_manual(values = c("~", "-", "`", "=",
  #                               "~", "-", "`", "=",
  #                               "~", "-", "`", "=",
  #                               "~", "-", "`", "=")) +
  guides(shape = FALSE,
         color = FALSE,
         size = FALSE)
