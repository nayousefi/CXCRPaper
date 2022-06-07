############# Melanocyte Wound Stats Analysis ########

############# By Roland Lacap and Nikolas Yousefi #########

############# Created on 2022-06-07 #########

### Load libraries

library(tidyverse)
library(ggpubr)
library(here)

### load data
melanocyte_wound <- read.csv(here("Data", "2022-06-06_Melanocyte_WOUNDS_Master_Sheet.csv"))

melanocyte_wound_pivot <- melanocyte_wound %>% 
  pivot_longer(cols = c(6:9),
               names_to = "Time_Period",
               values_to = "Values")

summary(melanocyte_wound_pivot)

melanocyte_anova <- aov(Values ~ siRNA + Time_Period, data = melanocyte_wound_pivot)

melanocyte_anova_sum <- summary(melanocyte_anova)

capture.output(melanocyte_anova_sum, file = "melanocyte_wound_anova_results_2022-06-07.doc")
