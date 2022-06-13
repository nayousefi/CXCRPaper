############# Melanocyte and Melanoma Wound Stats Analysis ########

############# By Roland Lacap and Nikolas Yousefi #########

############# Created on 2022-06-08 #########

### Load libraries

library(tidyverse)
library(ggpubr)
library(here)

melanocyte_wound <- read.csv(here("Data", "2022-06-06_Melanocyte_WOUNDS_Master_Sheet.csv"))

melanoma_wound <- read.csv(here("Data", "2022-06-02_Melanoma_WOUNDS_Master_Sheet.csv"))

melanocyte_wound_pivot <- melanocyte_wound %>% 
  pivot_longer(cols = c(6:9),
               names_to = "Time_Period",
               values_to = "Values")

melanoma_wound_pivot <- melanoma_wound %>% 
  pivot_longer(cols = c(6:9),
               names_to = "Time_Period",
               values_to = "Values")

melanocyte_melanoma_wounds <- full_join(melanocyte_wound_pivot, melanoma_wound_pivot, copy = FALSE) %>% 

summary(melanocyte_melanoma_wounds)

melanocyte_melanoma_anova <- aov(Values ~ Cell_type + Time_Period, data = melanocyte_melanoma_wounds)

melanocyte_melanoma_anova_sum <- summary(melanocyte_melanoma_anova)

capture.output(melanocyte_melanoma_anova_sum, file = "melanocyte_melanoma_wound_anova_results_2022-06-08.doc")

melanocyte_melanoma_wounds %>% 
  write.csv(here("Data", "melanocyte_melanoma_wounds.csv"))
