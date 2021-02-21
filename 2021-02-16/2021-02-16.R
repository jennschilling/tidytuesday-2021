# Author : Jenn Schilling
# Title: #TidyTuesday Du Bois Challenge
# Date: Feb 16 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(pBrackets)
library(grid)
library(ggforce)
library(patchwork)

#### Fonts ####

title_font <- "Consolas"
axis_font <- "Microsoft Sans Serif" # Alternate: "Gill Sans MT" 

# Microsoft has a better match for 9 best, Gill has a better match for 1

#### Challenge 1 ####

# Target Plot: https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge01/original-plate-07.jpg

# Colors 

background <- "#E9D1B4"
grid_line <- "#EFBEA3" #"#cc7f7f"
font_color <- "gray50"

# Data

georgia_pop <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/georgia_pop.csv')

georgia_pop_long <- georgia_pop %>%
  pivot_longer(cols = Colored:White,
               names_to = 'race',
               values_to = 'percent') %>%
  rename(year = Year)

# Plot

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


#### Challenge 2 #####

# Target Plot: https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge02/original-plate-10.jpg

# Colors

background <- "#e5d4c3"
font_color <- "#3B3326"
label_color <- "gray40"

widow_divorce <- "#4b6350"
married <- "#f3b21a"
single <- "#d13345"

conjugal_colors <- c(widow_divorce, married, single)

# Data

conjugal <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/conjugal.csv')

conjugal_long <- conjugal %>%
  pivot_longer(cols = Single:`Divorced and Widowed`,
               names_to = "conjugal_category",
               values_to = "percent") %>%
  janitor::clean_names() %>%
  mutate(conjugal_category = ifelse(conjugal_category == "Divorced and Widowed",
                                    "WIDOWED AND DIVORCED",
                                    toupper(conjugal_category))) %>%
  mutate(conjugal_category = factor(conjugal_category, 
                                    levels = c("WIDOWED AND DIVORCED",
                                               "MARRIED",
                                               "SINGLE"))) %>%
  mutate(population = toupper(population)) %>%
  mutate(population = factor(population,
                             levels = c("NEGROES", "GERMANY"))) %>%
  mutate(age = toupper(age),
         age = ifelse(age == "60 AND OVER",
                      "60\nAND\nOVER", age)) %>%
  mutate(label = ifelse(percent == 0.6, 
                        paste0(".6", "\n", "%"),
                        paste0(percent, "%")),
         label_x = ifelse(conjugal_category == "SINGLE",
                          percent / 2,
                          ifelse(conjugal_category == "WIDOWED AND DIVORCED",
                                 100 - percent/2,
                                 lag(percent, n = 1) + percent / 2 )))


# Plot

# Main Plot
c2 <- 
  ggplot(data = conjugal_long,
       mapping = aes(x = percent,
                     y = population,
                     fill = conjugal_category)) +
  
  geom_col(width = 0.5,
           color = font_color,
           key_glyph = draw_key_point) +
  
  coord_cartesian(clip = "off", expand = FALSE) +
  
  facet_wrap(~ age,
             ncol = 1,
             strip.position = "left") +
  
  geom_text(data = conjugal_long,
            mapping = aes(x = label_x, label = label),
            family = axis_font,
            color = font_color,
            size = 3.5) +
  
  scale_fill_manual(values = conjugal_colors) +
  
  guides(fill = FALSE) +

  labs(x = "",
       y = "") +
  
  geom_text(data = tibble(x = c(0, 0, 0),
                          y = c(0.7, 0.7, 0.7),
                          age = c("15-40", "40-60", "60\nAND\nOVER"),
                          lab = c("15-40           ", "40-60           ", "60 AND OVER"),
                          conjugal_category = c("WIDOWED AND DIVORCED",
                                                "MARRIED",
                                                "SINGLE")),
            mapping = aes(x = x, y = y, label = lab),
            family = axis_font,
            color = label_color,
            size = 2.5,
            hjust = 1.15) +
  
  geom_text(data = tibble(x = c(0, 0, 0),
                          y = c(2, 0.7, 0.7),
                          age = c("15-40", "40-60", "60\nAND\nOVER"),
                          lab = c("AGE", "", ""),
                          conjugal_category = c("WIDOWED AND DIVORCED",
                                                "MARRIED",
                                                "SINGLE")),
            mapping = aes(x = x, y = y, label = lab),
            family = axis_font,
            color = label_color,
            size = 4,
            hjust = 4.75) +
    
  theme(axis.ticks = element_blank(),
        
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = axis_font, 
                                   color = label_color,
                                   size = 11),
        
        strip.text.y.left = element_text(family = axis_font,
                                         color = label_color,
                                         size = 11,
                                         angle = 0),
        
        strip.placement = "outside",
        
        strip.switch.pad.wrap = unit(0.75, "cm"),
        
        strip.background = element_blank(),
        
        panel.spacing.y = unit(2.5, "cm"),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill =  background, color = NA),
        panel.background = element_rect(fill = background, color = NA),
        
        plot.margin = margin(t = 10, r = 30, b = 40, l = 20),
        
        plot.title.position = "plot")


# Title and Legend
c2_annotations <- ggplot() +
  annotate("point", x = c(0, 0, 0.2), y = c(0.1, -0.1, 0), 
           shape = 21, size = 12, fill = c(single, married, widow_divorce), 
           color = font_color) +
  annotate("text", x = c(0.032, 0.034, 0.27), y = c(0.1, -0.1, 0),
           label = c("SINGLE", "MARRIED", "WIDOWED AND DIVORCED"), 
           family = axis_font, color = label_color) +
  scale_x_continuous(limits = c(0, 0.33)) +
  scale_y_continuous(limits = c(-0.2, 0.2)) +
  coord_cartesian(clip = "off", expand = FALSE) +
  labs(title = "CONJUGAL CONDITION.") +
  theme_void() +
  theme(plot.margin = margin(t = 10, r = 200, b = 20, l = 200),
        plot.title = element_text(family = title_font, size = 20,
                                 face = "bold", hjust = 0.5,
                                 margin = margin(t = 0, r = 0, b = 30, l = 0)),
        plot.background = element_rect(fill =  background, color = NA))

# Brackets

b1 <- bracketsGrob(-0.11, 1, -0.11, 0, h = -0.3, lwd = 1, 
                  col = label_color)


c2_brackets <- c2 +
  annotation_custom(b1) 

c2_final <- (c2_annotations / c2_brackets) +
  plot_layout(heights = c(1, 4)) +
  plot_annotation(caption = "Source: W.E.B. Du Bois' Data Portraits #DuBoisChallenge | Viz: Jenn Schilling") &
  theme(plot.background = element_rect(fill =  background, color = NA),
        plot.caption = element_text(family = axis_font, color = label_color, hjust = 1)) 

# placement of text under "Negroes" is not quite right



#### Challenge 7 ####

# Target Plot: https://github.com/ajstarks/dubois-data-portraits/blob/master/challenge/challenge07/original-plate-25.jpg

# Colors

background <- "#E7D5C3"
font_color <- "gray50"

color_75 <- "#F0B7AE"
color_80 <- "#A8A7B7"
color_85 <- "#B99B85"
color_90 <- "#F1B73F"
color_95 <- "#D4C5B3"
color_99 <- "#D83448"

bar_colors <- c(color_75, color_80, color_85, 
                color_90, color_95, color_99)

# Data

furniture <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/furniture.csv')

furniture <- furniture %>% 
  janitor::clean_names() %>%
  mutate(year = as.factor(year))


# Make Spiral Plot Data


# Plot

ggplot(data = furniture,
       mapping = aes(x = year,
                     y = houshold_value_dollars,
                     fill = year)) +
  geom_col() +
  scale_fill_manual(values = bar_colors) 


  