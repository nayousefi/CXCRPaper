library(tidyverse)
library(here)

area <- read_csv((here("2021-01-08", "Area_ANOVA", "Data", "Area-2021-01-08-FORANOVA.csv")))

View(area)

summary(area)

areaanova <- aov(Area_mc ~ Type + Antibody, data = area)

areaanovasum <- summary(areaanova)

capture.output(areaanovasum, file = "area_anova_results_2021-01-08.doc")
