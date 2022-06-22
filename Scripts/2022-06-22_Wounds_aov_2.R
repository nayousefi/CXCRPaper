### Plots pt 2 for wound assays
### Created by Roland Lacap
### Created on 2022-06-22

### Load Libraries
library(tidyverse)
library(here)
library(ggpubr)
library(afex)
library(ggsignif)

### Load data

melanocytes <- read.csv(here("Data", "2022-06-06_Melanocyte_WOUNDS_Master_Sheet.csv"))
melanoma <- read.csv(here("Data", "2022-06-02_Melanoma_WOUNDS_Master_Sheet.csv"))

wounds <- full_join(melanoma, melanocytes, copy = FALSE)

wounds2 <- wounds %>% 
  mutate(Cell_type, X = row_number()) %>% 
  mutate(HOUR_5,
         HOUR_5 = replace(HOUR_5, X == 570, 549102.6667)) %>% 
  mutate(HOUR_0,
         HOUR_0 = replace(HOUR_0, X == 89, 546769.6316)) %>% 
  mutate(HOUR_8,
         HOUR_8 = replace(HOUR_8, X == 408, 497188)) %>% 
  mutate(HOUR_8,
         HOUR_8 = replace(HOUR_8, X == 412, 467957.4)) %>% 
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
                        "CXCR47_siRNA" = "CXCR4 & 7 siRNA")) %>% 
  mutate(Treatment = recode(Treatment,
                        "No_SDF1" = "-SDF1",
                        "SDF1" = "+SDF1")) %>% 
  unite(Sample, c(siRNA,Cell_type,Treatment), sep = " ", remove = FALSE) %>% 
  unite(Cond, c(siRNA,Treatment), sep = " ", remove = FALSE) %>%
  group_by(Sample,Cell_type) %>% 
  summarize(avg0 = mean(HOUR_0),
            avg5 = mean(HOUR_5),
            avg8 = mean(HOUR_8),
            avg18 = mean(HOUR_18)) %>% 
      mutate(HOUR_0A = (avg0/avg0),
             HOUR_5A = (avg5/avg0),
             HOUR_8A = (avg8/avg0),
             HOUR_18A = (avg18/avg0)) 

wounds_long <- wounds2 %>% 
  select(1,2,7:10) %>% 
  pivot_longer(cols = c(3:6),
               names_to = "Time_Period",
               values_to = "Values")
  
  
  
  #mutate(Cond = factor(Cond, levels = 
   #                       c("Normal -SDF1","Normal +SDF1",
    #                        "Scramble -SDF1","Scramble +SDF1",
     #                       "CXCR4 siRNA -SDF1","CXCR4 siRNA +SDF1",
      #                      "CXCR7 siRNA -SDF1","CXCR7 siRNA +SDF1",
       #                     "CXCR4 & 7 siRNA -SDF1","CXCR4 & 7 siRNA +SDF1")))

melanoma2 <- wounds_long %>% 
  filter(Cell_type == "Melanoma") %>%
  mutate(Sample = factor(Sample, levels =
                           c("Normal Melanoma -SDF1","Normal Melanoma +SDF1",
                             "Scramble Melanoma -SDF1","Scramble Melanoma +SDF1",
                             "CXCR4 siRNA Melanoma -SDF1","CXCR4 siRNA Melanoma +SDF1",
                             "CXCR7 siRNA Melanoma -SDF1","CXCR7 siRNA Melanoma +SDF1",
                             "CXCR4 & 7 siRNA Melanoma -SDF1","CXCR4 & 7 siRNA Melanoma +SDF1")))

aov_comparisons <-  
  list(c())

p_melanoma <- melanoma2 %>% 
  ggplot(aes(Sample, Values, fill = Sample))+
  geom_bar(stat = "identity")+
  facet_wrap(~Time_Period, ncol = 4)

p_melanoma

