# Author : Jenn Schilling
# Title: #TidyTuesday Star Trek commands
# Date: August 17 2021

#### Libraries ####

library(tidyverse)
library(extrafont)
library(ggtext)

#### Data #### 

computer <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

computer_plot <- computer %>%
  mutate(type = str_to_sentence(type),
         domain = str_to_sentence(domain)) %>%
  count(type, domain, char_type) %>%
  mutate(domain = ifelse(is.na(domain), "Other", 
                         ifelse(domain == "Iot", "IoT",
                          ifelse(domain == "Infoseek", "InfoSeek", domain))))

computer_plot <- computer_plot %>%
  mutate(domain = factor(domain, levels = c("Analysis", "Communications", "Emergency", "Entertainment",
                                            "Help", "InfoSeek", "IoT", "Override", "Other")),
         type = factor(type, levels = unique(computer_plot$type)))  

#### Formatting ####

font <- "Consolas"
titlefont <- "Consolas"
fontcolor <- "#FFFFFF"
bcolor <- "#343434"

star_trek_yellow <- "#EBC41F"
star_trek_blue <- "#01A0D3"
start_trek_red <- "#DC3A51"

theme_set(theme_minimal(base_size = 12, base_family = font))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  
  panel.background = element_rect(fill = bcolor, color = NA),
  plot.background = element_rect(fill = bcolor, color = NA),
  
  axis.title = element_text(size = 14, color = fontcolor),
  axis.text = element_text(size = 12, color = fontcolor),
  axis.ticks = element_line(color = fontcolor),
  
  axis.line = element_line(color = fontcolor),
  
  strip.text = element_text(size = 10, color = fontcolor, hjust = 0),
  
  legend.text = element_text(size = 12, color = fontcolor),
  legend.title = element_text(size = 14, color = fontcolor, lineheight = 0.5),
  
  plot.title.position = "plot",
  plot.title = element_markdown(size = 16, color = fontcolor, family = titlefont),
  
  plot.subtitle = element_markdown(size = 14, color = fontcolor, family = font, lineheight = 1.25),
  
  plot.caption.position = "plot",
  plot.caption = element_markdown(size = 10, color = fontcolor, hjust = 1),
  
  plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
)

#### Plot ####

ggplot(mapping = aes(x = domain,
                     y = type,
                     size = n,
                     shape = char_type,
                     color = char_type)) +
  geom_point(data = computer_plot %>% 
                filter(type != "Conversation" | char_type != "Computer")) +
  geom_point(data = computer_plot %>% 
               filter(type == "Conversation" & char_type == "Computer")) +
  scale_y_discrete(limits = rev(levels(computer_plot$type))) +
  scale_x_discrete(position = "top") +
  scale_color_manual(values = c(start_trek_red, star_trek_blue),
                     guide = guide_legend(title.position = "top",
                                          keywidth = 1.5,
                                          override.aes = list(size = 4))) +
  scale_shape_manual(values = c(19, 17)) +
  scale_size_continuous(range = c(4, 12)) +
  guides(size = "none") +
  labs(title = "Interactions with the Enterprise's computer in Star Trek: The Next Generation<br>",
       subtitle = "Size indicates the number of interactions in the domain-type combination during the series.<br>
       The number of interactions ranges from 1 for Analysis-Clarification to 176 for Entertainment-Command.<br>",
       x = "Domain\n",
       y = "Type",
       color = "Who Spoke?\n",
       shape = "Who Spoke?\n",
       caption = "<b>Data:</b> SpeechInteraction.org. | <b>Design:</b> Jenn Schilling") +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.justification = "left")


# Save
ggsave("2021-08-17\\startrek.png",
       plot = last_plot(),
       device = "png",
       width = 13,
       height = 10,
       type = "cairo")  
