# Author : Jenn Schilling
# Title: #TidyTuesday Plastics
# Date: Jan 26 2021

#### Libraries ####

library(tidyverse)
library(skimr)

#### Get the Data ####

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

#### Explore the Data ####

skim(plastics)