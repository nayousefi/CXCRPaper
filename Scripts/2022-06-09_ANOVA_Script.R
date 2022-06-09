### Creating Plot for Wound Assays 
### Created by Roland Lacap & Nikolas Yousefi
### Created on 2022-06-09

### Load Libraries
library(tidyverse)
library(here)
library(ggpubr)
library(ggstatsplot)
library(afex)
library(ggsignif)

### Load Data
wounds <- read.csv(here("Data","melanocyte_melanoma_wounds.csv"))

### Wrangle Data
wounds1 <- wounds %>% 
  mutate(Time_Period = factor(Time_Period,
                              levels = c("HOUR_0",
                                         "HOUR_5",
                                         "HOUR_8",
                                         "HOUR_18"))) %>% 
  mutate(Values,
         Values = replace(Values, X == 1126, 549102.6667)) %>%  #had to change value to average
  mutate(siRNA = factor(siRNA,
                        levels = c("Unstransfected",
                                   "Scramble",
                                   "CXCR4_siRNA",
                                   "CXCR7_siRNA",
                                   "CXCR47_siRNA"))) %>% 
  mutate(siRNA = recode(siRNA,
                        "Unstransfected" = "Normal",
                        "CXCR4_siRNA" = "CXCR4 siRNA",
                        "CXCR7_siRNA" = "CXCR7 siRNA",
                        "CXCR47_siRNA" = "CXCR4 & 7 siRNA"))

### plot data
p <- wounds1 %>% 
  ggwithinstats(x = Time_Period, y = Values)

p

### Compare means
comparisons <- wounds1 %>% 
  compare_means()

p2 <- wounds1 %>% 
  ggboxplot(x = "Time_Period", y = "Values", 
           fill = "siRNA")+
  geom_signif()+
  facet_wrap(~Cell_type+Treatment)

p2 + stat_compare_means(label = "p.signif", method = "anova")
