#### 1. Environment setup ####
library(tidyverse)
library(ggsci)
library(ggpubr)
library(ComplexHeatmap)
library(circlize)
source('tools.R')
#### 2. Load data #### 
age <- read_csv('data/cancer-deaths-by-age.csv')
col_jama = ggsci::pal_jama(palette = "default")(7)

#### 3. Age distribution ####

incidence <- read_csv("data/clean/incidence.csv")

age %>% 
  mutate(Rate = as.numeric(str_replace(Rate,"S","0"))) %>%
  # filter(Number > 5) %>%
  mutate(population = ifelse(agegrpid <= 3, "child",'adult')) %>%
  group_by(population) %>%
  summarize(sum = sum(Number), mean = mean(Rate))

#### 4. Gender variance of cancer types ####
incidence <- read_csv("data/clean/incidence.csv")
mortality <- read_csv('data/clean/mortality.csv')
g1 =  draw_sex_variance(incidence,type="Incidence",fulltitle=FALSE)
g2 = draw_sex_variance(mortality,type="Mortality",fulltitle=FALSE)

ggarrange(g1,g2)

#### 5. Regional distribution  #####
incidence_sexfiltered <- read_csv("data/clean/incidence_sexfiltered.csv")
mortality_sexfiltered <- read_csv('data/clean/mortality_sexfiltered.csv')

ht_incidence = regional_heatmap(incidence_sexfiltered,type='Incidence')
draw(ht_incidence, heatmap_legend_side = "right",annotation_legend_side="right",legend_grouping = "original")


ht_mortality = regional_heatmap(mortality_sexfiltered,type='mortality')
draw(ht_mortality, heatmap_legend_side = "right",annotation_legend_side="right",legend_grouping = "original")



#### 6. Most common region for each cancer ####


incidence








