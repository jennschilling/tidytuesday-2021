# Author : Jenn Schilling
# Title: #TidyTuesday Employment and Earnings
# Date: Feb 23 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(patchwork)

#### Fonts ####


#### Data ####

employed <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv')

earn <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')
