# Author : Jenn Schilling
# Title: #TidyTuesday Transit Costs
# Date: Jan 5 2021

#### Libraries ####

library(tidyverse)
library(skimr)

#### Get the Data ####

transit_cost <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

# Remove the last lines that are summary stats
transit_cost_sub <- transit_cost[1:537, ]

#### Explore the Data ####

skim(transit_cost_sub)

# Make Percent of Line Completed (tunnel_per) and Real Cost numeric
transit_cost_sub <- transit_cost_sub %>%
  mutate(tunnel_per = parse_number(tunnel_per),
         real_cost = parse_number(real_cost))
