library(tidyverse)
library(here)

AdjIntDen <- read_csv((here("2021-01-08", "Intensity_ANOVA", "Data", "Intensity-2021-01-08-FORANOVA.csv")))

View(AdjIntDen)

summary(AdjIntDen)

intanova <- aov(AdjustedIntegratedDensity ~ Type + Antibody, data = AdjIntDen)

intanovasum <- summary(intanova)

capture.output(intanovasum, file = "intensity_anova_results_2021-01-08.doc")
