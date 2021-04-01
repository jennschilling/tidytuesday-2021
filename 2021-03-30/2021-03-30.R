# Author : Jenn Schilling
# Title: #TidyTuesday Makeup Shades
# Date: Mar 30 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(patchwork)
library(ggtext)

#### Data #### 

allCategories <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allCategories.csv')
allShades <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')

#### Formatting ####

font <- "Gill Sans MT"
fontcolor <- "gray30"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  
  axis.title = element_text(size = 10, color = fontcolor),
  axis.text = element_text(size = 9, color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 12, color = fontcolor),
  
  plot.subtitle = element_markdown(size = 10, color = fontcolor),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 8, color = fontcolor),
  
  plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
)

#### Plot ####


# Distribution of All Shades

all_shades <- allShades %>%
  mutate(lightness_group = cut_interval(lightness, 20)) %>%
  group_by(lightness_group) %>%
  arrange(sat, .by_group = TRUE) %>%
  mutate(y = row_number()) %>%
  ungroup()

all_plot <- ggplot(data = all_shades,
       mapping = aes(x = lightness_group,
                     y = y,
                     fill = hex)) +
  geom_tile() +
  scale_fill_identity() +
  guides(fill = FALSE) +
  labs(title = "The distribution of foundation shades from 107 brands at Ulta and Sephora shows that there<br>
       are more options for lighter complexions.",
       x = "",
       y = "") +
  coord_cartesian(expand = FALSE) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank())

# Distribution by Category

all_categories <- allCategories %>%
  left_join(allShades, by = c("brand", "product", "url", 
                              "imgSrc", "name", "specific", 
                              "hex", "lightness")) %>%
  separate(categories, into = c("cat1", "cat2", "cat3", "cat4"), sep = ", ") %>%
  pivot_longer(cat1:cat4, 
               names_to = "label",
               values_to = "category",
               values_drop_na = TRUE) %>%
  select(-label) %>%
  filter(category != "sand") %>% # only 1 record
  mutate(lightness_group = cut_interval(lightness, 20),
         category = str_to_sentence(category)) %>%
  group_by(category, lightness_group) %>%
  arrange(sat, .by_group = TRUE) %>%
  mutate(y = row_number()) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(category = paste0(category, " (", format(n(), big.mark = ","), ")\n")) %>%
  ungroup()

cat_plot <- ggplot(data = all_categories,
       mapping = aes(x = lightness_group,
                     y = y,
                     fill = hex)) +
  geom_tile() +
  scale_fill_identity() +
  facet_wrap(~category,
             scales = "free_y") +
  guides(fill = FALSE) +
  labs(title = "When foundation shades are categorized by their label, the distributions by category shows that<br>
       **drink** and **wood** categories are more frequently used for darker shades while **gem** and **skin**<br>
       categories are more frequently used for lighter shades.",
       x = "",
       y = "",
       caption =  "Data: <b>The Pudding</b> | Viz: <b>Jenn Schilling</b>") +
  coord_cartesian(expand = FALSE) +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(0.5, "cm"))

# Put Plots Together

all_plot / cat_plot +
  plot_layout(heights = c(0.40, 0.60))


ggsave("2021-03-30\\foundation_shades.png",
       plot = last_plot(),
       device = "png",
       width = 7,
       height = 9,
       dpi = 500,
       type = "cairo")

