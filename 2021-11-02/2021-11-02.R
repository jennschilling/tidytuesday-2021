# Author : Jenn Schilling
# Title: #TidyTuesday Making Maps with R
# Date: November 2 2021

#### Libraries ####

library(sf)
library(raster)
library(dplyr)
library(spData)
# install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge) 
library(tmap)   
library(leaflet) 
library(ggplot2) 

#### Plot ####

# From: https://geocompr.robinlovelace.net/adv-map.html

# Add fill layer to nz shape
tm_shape(nz) +
  tm_fill() 
# Add border layer to nz shape
tm_shape(nz) +
  tm_borders() 
# Add fill and border layers to nz shape
tm_shape(nz) +
  tm_fill() +
  tm_borders() 