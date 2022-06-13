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
### Statistics
#melanoma overall
melanoma_stats<-wounds1 %>% 
  filter(Cell_type != "Melanocytes") %>% 
  select(siRNA,Values,Treatment,Time_Period) %>% 
  group_by(Time_Period)

table1 <- 
  table(melanoma_stats$Treatment, melanoma_stats$siRNA)

tibble(table1)

melanoma_aov <- 
  aov(Values ~ siRNA * Treatment, data = melanoma_stats)
summary(melanoma_aov)

#melanoma + sdf1
melanoma_sdf1_stats<-wounds1 %>% 
  filter(Cell_type != "Melanocytes",
         Treatment != "No_SDF1") %>% 
  select(siRNA,Values,Time_Period) %>% 
  group_by(Time_Period)

table2 <- 
  table(melanoma_sdf1_stats$Time_Period, melanoma_sdf1_stats$siRNA)

tibble(table2)

melanoma_sdf1_aov <- 
  aov(Values ~ siRNA * Time_Period, data = melanoma_sdf1_stats)
summary(melanoma_sdf1_aov)

#melanoma no sdf1
melanoma_no_sdf1_stats<-wounds1 %>% 
  filter(Cell_type != "Melanocytes",
         Treatment != "SDF1") %>% 
  select(siRNA,Values,Time_Period) %>% 
  group_by(Time_Period)

table3 <- 
  table(melanoma_no_sdf1_stats$Time_Period, melanoma_no_sdf1_stats$siRNA)

tibble(table3)

melanoma_no_sdf1_aov <- 
  aov(Values ~ siRNA * Time_Period, data = melanoma_no_sdf1_stats)
summary(melanoma_no_sdf1_aov)

#melanocytes
melanocytes_stats<-wounds1 %>% 
  filter(Cell_type != "Melanoma") %>% 
  select(siRNA,Treatment,Values,Time_Period) %>% 
  group_by(Time_Period) %>% 
  mutate(Treatment = factor(Treatment,
                            levels = c("No_SDF1","SDF1")))
table4 <- 
  table(melanocytes_stats$Treatment, melanocytes_stats$siRNA)

tibble(table4)

melanocytes_aov <- 
  aov(Values ~ siRNA * Treatment, data = melanocytes_stats)
summary(melanocytes_aov)

#melanocytes + sdf1
melanocytes_sdf1_stats<-wounds1 %>% 
  filter(Cell_type != "Melanoma",
         Treatment != "No_SDF1") %>% 
  select(siRNA,Values,Time_Period) %>% 
  group_by(Time_Period)

table5 <- 
  table(melanocytes_sdf1_stats$Time_Period, melanocytes_sdf1_stats$siRNA)

tibble(table5)

melanocytes_sdf1_aov <- 
  aov(Values ~ siRNA * Time_Period, data = melanocytes_sdf1_stats)
summary(melanocytes_sdf1_aov)


#melanocytes no sdf1
melanocytes_no_sdf1_stats<-wounds1 %>% 
  filter(Cell_type != "Melanoma",
         Treatment != "SDF1") %>% 
  select(siRNA,Values,Time_Period) %>% 
  group_by(Time_Period)

table6 <- 
  table(melanocytes_no_sdf1_stats$Time_Period, melanocytes_no_sdf1_stats$siRNA)

tibble(table6)

melanocytes_no_sdf1_aov <- 
  aov(Values ~ siRNA * Time_Period, data = melanocytes_no_sdf1_stats)
summary(melanocytes_no_sdf1_aov)

### plot data

p2 <- wounds1 %>% 
  ggboxplot(x = "Time_Period", y = "Values", 
           fill = "siRNA")+
  facet_wrap(~Cell_type+Treatment)

p2 + stat_compare_means()

