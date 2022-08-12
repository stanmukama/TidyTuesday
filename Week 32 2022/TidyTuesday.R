#TidyTuesday Week 32
install.packages("tidytuesdayR")
install.packages("ggvsg")
remotes::install_github("emilhvitfeldt/ferriswheels")

#loading libraries 
library(tidytuesdayR)
library(ferriswheels)
library(ggplot2)
library(tidyverse)
library(ggrepel)

#download Week 32 dataset
tuesdata <- tidytuesdayR::tt_load(2022, week = 32)


#summary data 
wheels <- tuesdata$wheels

#Keeping the tallest ferris wheel by country
wheels <- wheels %>% 
  select(height, country, diameter, name) %>%
  na.exclude() %>%
  group_by(country) %>%
  filter(height == max(height))

#plot theme
plot_theme <- theme(panel.background = element_blank(),
      plot.background = element_rect(fill = "azure1"),
      plot.title = element_text(face = "bold", size = 15),
      panel.grid = element_blank(),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 13),
      axis.text.x = element_text(size = 12),
      axis.line = element_line(),
      legend.position = "top",
      legend.text = element_text(size = 13),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(size = 13),
      plot.caption = element_text(size = 11, face = "italic"))

#the plot
wheels %>% 
  ggplot(aes(x = fct_reorder(country, height), y =height))+
  geom_point(aes(size=diameter),shape = "\U1F3A1", colour = "black", fill = "white", stroke = 12)+
  scale_size_continuous(range = c(1, 8))+
  scale_y_continuous(limits = c(150,800))+
  labs(y = "Height of the ferris wheels (metres)",
       x = "Country",
       size = "Diameter (metres)",
       title = "Tallest and largest ferris wheels by country",
       subtitle = "US and China dominate the ferris wheels world",
       caption = 'Trasias Mukama | #TidyTuesday Week 32 | Data: @Emil_Hvitfeldt ferris wheels')+
    geom_text_repel(data = wheels, aes(label = name),box.padding = 1.0, nudge_y = 1.0, direction = "y", hjust = "top") + 
    plot_theme

#save 
ggsave("Week32_ferris_wheels.tiff")


