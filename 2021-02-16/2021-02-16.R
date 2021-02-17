# Author : Jenn Schilling
# Title: #TidyTuesday Du Bois Challenge
# Date: Feb 16 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(pBrackets)
library(grid)

#### Fonts ####

title_font <- "Consolas"
axis_font <- "Microsoft Sans Serif" # Alternate: "Gill Sans MT" 

# Microsoft has a better match for 9 best, Gill has a better match for 1

#### Colors ####

background <- "antiquewhite2"
grid_line <- "#cc7f7f"
font_color <- "gray50"

#### Challenge 1 ####

# Target Plot: https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge01/original-plate-07.jpg

# Data
georgia_pop <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')

georgia_pop_long <- georgia_pop %>%
  pivot_longer(cols = Colored:White,
               names_to = 'race',
               values_to = 'percent') %>%
  rename(year = Year)


c1 <- ggplot(georgia_pop_long,
       aes(x = percent,
           y = year,
           linetype = toupper(race))) +
  
  geom_path() +
  
  coord_cartesian(clip = "off") +
  
  scale_x_reverse(limits = c(100, 0),
                  breaks = seq(0, 100, 5),
                  expand = c(0, 0)) +
  
  scale_y_continuous(limits = c(1790, 1890),
                     breaks = seq(1790, 1890, 10),
                     expand = c(0, 0)) +
  
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("= COLORED", "= WHITE")) +
  
  labs(title = "COMPARATIVE INCREASE OF WHITE AND COLORED\nPOPULATION OF GEORGIA.\n\n",
       x = "",
       y = "",
       linetype = "") + 
  
  theme(legend.background = element_rect(fill = background, color = NA),
        legend.key = element_rect(fill = background, color = NA),
        legend.key.width = unit(2, unit = "cm"),
        legend.text = element_text(margin = margin(r = 4, unit = "cm"), 
                                   size = 7, color = font_color),
        legend.box.margin = margin(l = 3, t = 0.5, unit = "cm"),
        legend.position = "bottom",
        
        axis.ticks = element_blank(),
        
        axis.text.x = element_text(family = axis_font, color = font_color, size = 7),
        axis.text.y = element_text(family = axis_font, color = font_color),
        
        panel.grid.major.x = element_line(color = grid_line),
        panel.grid.major.y = element_line(color = grid_line),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        plot.background = element_rect(fill =  background, color = NA),
        panel.background = element_rect(fill = background, color = NA),
        panel.border = element_rect(fill = NA, color = font_color),
        
        plot.margin = margin(t = 10, r = 25, b = 10, l = 25),
        
        plot.title = element_text(family = title_font, size = 12,
                                  face = "bold", hjust = 0.5),
        
        plot.title.position = "plot"
        
        )

# Add Curly Brace
# Source: https://stackoverflow.com/questions/35633239/add-curly-braces-to-ggplot2-and-then-use-ggsave

bracketsGrob <- function(...){
  l <- list(...)
  e <- new.env()
  e$l <- l
  grid:::recordGrob(  {
    do.call(grid.brackets, l)
  }, e)
}


b <- bracketsGrob(1.01, -0.03, -0.01, -0.03, h = 0.05, lwd = 1, 
                  col = font_color)

t <- textGrob("PERCENTS", x = 0.5, y = -0.1,
              gp = gpar(fontfamily = axis_font, 
                        fontsize = 7,
                        col = font_color))

c1_final <- c1 + 
  annotation_custom(b) +
  annotation_custom(t)

ggsave("2021-02-16\\c1_final.png",
       plot = c1_final,
       device = "png",
       width = 5,
       height = 7,
       dpi = 300,
       type = "cairo")
