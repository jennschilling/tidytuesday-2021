# Author : Jenn Schilling
# Title: #TidyTuesday Tate Art Museum
# Date: Jan 12 2021

#### Libraries ####

library(tidyverse)
library(magick)
library(imager)

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
  
  return(tmp %>% select(hex)) 
  
}

get_colorPal(turner_oil_canvas$thumbnailUrl[1]) 

# Sort by year, get the thumbnail URL
# Set number of colors to 5 and colorspace to RGB
# Remove records with no URL
# Remove record with Not Found error for the URL
turner_oil_canvas_input <- turner_oil_canvas %>%
  arrange(year) %>%
  select(thumbnailUrl) %>%
  rename(url = thumbnailUrl) %>%
  mutate(n = 5,
         cs = "RGB") %>%
  filter(!is.na(url)) %>%
  filter(url != 'http://www.tate.org.uk/art/images/work/N/N03/N03386_8.jpg')

# For each image - read it and find the colors
turner_oil_canvas_output <- pmap_df(turner_oil_canvas_input, get_colorPal)
