# Author : Jenn Schilling
# Title: #TidyTuesday Tate Art Museum
# Date: Jan 12 2021

#### Libraries ####

library(tidyverse)
library(magick)
library(imager)
library(DescTools)

#### Get the Data ####

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

#### Get the Turners ####

turner <- artists %>%
  filter(str_detect(name, "Turner, Joseph Mallord William")) 

turner_art <- artwork %>%
  right_join(., turner, by = c("artistId" = "id"))

turner_oil_canvas <- turner_art %>%
  filter(str_detect(medium, "Oil paint on canvas"))

#### Get colors from thumbnail images ####


# Code Source: https://chichacha.netlify.app/2019/01/19/extracting-colours-from-your-images-with-image-quantization/
get_colorPal <- function(url, n=8, cs="RGB"){
  
  im <- image_read(url) # added to read the image inside the function
  
  tmp <-im %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reduce colors 
    magick2cimg() %>%  ## prep for making data frame
    as.data.frame(wide="c") %>%  ## making it wide 
    mutate(hex=rgb(c.1,c.2,c.3)) %>%
    count(hex, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(hex) %>% mutate(url = url)) 
  
}

get_colorPal(turner_oil_canvas$thumbnailUrl[1]) # Test

turner_oil_canvas_input <- turner_oil_canvas %>%
  arrange(year) %>% # sort by year
  select(thumbnailUrl) %>% # get thumbnail URL
  rename(url = thumbnailUrl) %>%
  mutate(n = 5, # set number of colors to 5
         cs = "RGB") %>% # set colorspace to RGB
  filter(!is.na(url)) %>% # remove records with no URL
  filter(url != 'http://www.tate.org.uk/art/images/work/N/N03/N03386_8.jpg') # remove record with file not found url

# For each image - read it and find the colors

# 10 records takes 8.1 seconds, so 240 should only take 3.2 minutes
turner_oil_canvas_output <- pmap_df(turner_oil_canvas_input, get_colorPal)

# Get hue for each color to organize visual
turner_oil_canvas_output <- turner_oil_canvas_output %>%
  mutate(hue = ColToHsv(hex)) # this doesn't work

# Re-join using URL to get years
turner_oil_canvas_output <- turner_oil_canvas_output %>%
  left_join(., 
            artwork %>% select(thumbnailUrl, year, title), 
            by = c("url" = "thumbnailUrl"))
