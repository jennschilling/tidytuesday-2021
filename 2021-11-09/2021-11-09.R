# Author : Jenn Schilling
# Title: #TidyTuesday afrimapr
# Date: November 9 2021

#### Libraries ####

library(ggplot2) 
library(tmap)
library(remotes)
library(afrilearndata)

#### Data ####



#### Plot ####

# Resource: https://afrimapr-book.netlify.app/index.html


tm <- tm_shape(afripop2020) +
  tm_raster(breaks = c(0, 2, 20, 200, 2000, 25000),
            title = "",
            palette = "RdPu") + 
  tm_shape(africountries) +
  tm_borders() +
  tm_shape(africapitals) +
  tm_dots(size = 0.2) +
  tm_layout(main.title = "Population Density in Africa in 2020 is generally\nconcentrated near country capitals.",
            inner.margins = c(left = 0, right = 0, top = 0.05, bottom = 0),
            frame = FALSE,
            legend.position = c("left", "bottom"),
            fontfamily = "Verdana",
            main.title.fontfamily = "Verdana",
            main.title.position = "left",
            main.title.size = 1,
            attr.outside = TRUE) +
  tm_credits(text = "Data: afrimapr | Design: Jenn Schilling",
             position = 0.9,
             just = "right") +
  tm_credits(text = "Country capitals are shown as points on the map.",
             position = c(0.9, 0.5),
             just = "right")

# Save
tmap_save(tm, "2021-11-09\\afrimap.png")
