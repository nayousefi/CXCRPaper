############# Melanoma Wound Stats Analysis ########

############# By Roland Lacap and Nikolas Yousefi #########

############# Created on 2022-06-06 #########

### Load libraries

library(tidyverse)
library(ggpubr)
library(here)

### load data
melanoma_wound <- read.csv(here("Data", "2022-06-02_Melanoma_WOUNDS_Master_Sheet.csv"))

melanoma_wound_pivot <- melanoma_wound %>% 
  pivot_longer(cols = c(6:9),
               names_to = "Time_Period",
               values_to = "Values")

summary(melanoma_wound_pivot)

melanoma_anova <- aov(Values ~ siRNA + Time_Period, data = melanoma_wound_pivot)

melanoma_anova_sum <- summary(melanoma_anova)

capture.output(melanoma_anova_sum, file = "melanoma_wound_anova_results_2022-06-06.doc")
