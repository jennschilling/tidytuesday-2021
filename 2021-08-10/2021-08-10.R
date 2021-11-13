# Author : Jenn Schilling
# Title: #TidyTuesday BEA Infrastructure Investment
# Date: August 10 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)
library(ggalluvial)

#### Data #### 

investment <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')
chain_investment <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')
ipd <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/ipd.csv')
