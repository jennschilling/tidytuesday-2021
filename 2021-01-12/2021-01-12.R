# Author : Jenn Schilling
# Title: #TidyTuesday Tate Art Museum
# Date: Jan 12 2021

#### Libraries ####

library(tidyverse)
library(skimr)

#### Get the Data ####

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")