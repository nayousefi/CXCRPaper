############# Boyden Chamber Analysis #############

############# By Nikolas Yousefi #########

############# Created on 2022-07-29 #########

### Load libraries

library(tidyverse)
library(ggpubr)
library(here)
library(ggeasy)

### Load data

boyden <- read.csv(here("Data", "2022-07-29_BoydenScrambleKDs_CSV.csv")) #Loading csv file

View(boyden)

boydenclean <- boyden %>%
  select(Type, Migrated_Cells) # Selecting only categories of interest for plotting purposes

View(boydenclean)

ggplot(boydenclean, aes(x=reorder(Type,-Migrated_Cells, na.rm = TRUE), # Reordering X values in descending order
                      y = Migrated_Cells,
                      fill = Type))+  #Setting up the basic plot
  geom_boxplot(outlier.shape = NA)+ # Adding a boxplot'
  geom_jitter(color="black", size=0.4, alpha=0.9) + # Adding jitter to visualize data points
  theme_bw() + # Black and white theme selection
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16), # Setting text sizes
        legend.position="none", # Removing the legend
        plot.title = element_text(size=25, hjust = 0.5),
        plot.subtitle = element_text(size=15, hjust = 0.5))+ # Setting title size and centering the title and subtitles 
  labs(x = "",
       y = "Migrated Cells",
       title = "Comparisons of Migrated Cells against Cell Knockdowns",
       subtitle = "Using Boyden Chamber Analysis")+ # Adding axis and plot titles
  ggsave(here("Output", "BoydenGraph_2022-07-29.png"),
         width = 11, height = 9) # Saving graph in appropriate repository
