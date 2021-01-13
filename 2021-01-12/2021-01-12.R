# Author : Jenn Schilling
# Title: #TidyTuesday Tate Art Museum
# Date: Jan 12 2021

#### Libraries ####

library(tidyverse)

library(magick)
library(imager)
library(DescTools)
library(scales)

# library(sysfonts)
# library(showtext)
# 
# # Font Setup
# font_add_google("Lato")
# showtext_auto()

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
    RGBtoHSV() %>% # to get hue
    as.data.frame(wide="c") %>%  ## making it wide 
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1) %>%
    count(hex, hue, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(hex, hue, n) %>% mutate(url = url)) 
  
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
turner_oil_canvas_output <- pmap_df(turner_oil_canvas_input, get_colorPal)

# Re-join to artwork using URL to get year and title
turner_oil_canvas_output <- turner_oil_canvas_output %>%
  left_join(., 
            artwork %>% select(thumbnailUrl, year, title), 
            by = c("url" = "thumbnailUrl"))


# Make a plot
# Code Source: https://chichacha.netlify.app/2019/01/19/extracting-colours-from-your-images-with-image-quantization/
turner_oil_canvas_output %>%  
  group_by(year) %>%
  mutate(ypos=row_number(hue)) %>%  ## alter stacking order
  ggplot(aes(x=year, y=ypos, fill=hex)) +
  geom_tile() +
  scale_fill_identity() +
  scale_y_continuous(breaks=NULL) +
 # theme_void() +
 #  coord_polar() +
 # expand_limits(y=-1) +
  theme_classic() + # added
  labs(title = "Most Used Colors in Turner's Oil Paint on Canvas Works by Year",
       subtitle = "For each work, the top five colors were extracted.",
       x = "Year",
       caption = "TidyTuesday 12 Jan 2021 | Data: Tate Art Museum | Designer: Jenn Schilling | jennschilling.me") +
  theme(axis.line.y = element_blank(),
        axis.title.y = element_blank())


# Save plot
ggsave("2021-01-12\\turner_oil_colors.png",
       plot = last_plot(),
       device = "png",
       width = 10,
       height = 6,
       dpi = 300)
