# Author : Jenn Schilling
# Title: #TidyTuesday Making Maps with R
# Date: November 2 2021

#### Libraries ####

library(sf)
library(spData)
library(tmap)   
library(ggplot2) 
library(tidycensus)
library(extrafont)
library(scales)

#### Data ####

# B19013_001
# Estimate Median household income in the past 12 months (in 2019 inflation-adjusted dollars)

mich <- get_acs(state = "Michigan",
                geography = "county", 
                variables = "B19013_001", 
                geometry = TRUE)

#### Plot ####

# Resource: https://walker-data.com/census-r/mapping-census-data-with-r.html

tm <- tm_shape(mich) +
  tm_polygons(col = "estimate",
              palette = "Blues",
              legend.format = c(prefix = "$"),
              title = "") +
  tm_layout(main.title = "Median Household Income\nby County in Michigan",
            frame = FALSE,
            legend.outside = FALSE,
            legend.hist.width = 5,
            fontfamily = "Verdana",
            main.title.fontfamily = "Verdana",
            main.title.position = "left",
            main.title.size = 1,
            attr.outside = TRUE) +
  tm_credits(text = "Data: 2015-2019 American Community Survey\nDesign: Jenn Schilling",
             position = "left")

# Save
tmap_save(tm, "2021-11-02\\michiganincome.png")
