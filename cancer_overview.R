#### 1. Environment setup ####
library(tidyverse)
library(ggsci)
library(ggpubr)
library(ggrepel)
library(sf)
library(flextable)
source('tools.R')

#### 2. Load data #### 
age <- read_csv('data/cancer-deaths-by-age.csv')


#### 3. Age distribution ####

incidence <- read_csv("data/clean/incidence.csv")

age %>% 
  mutate(Rate = as.numeric(str_replace(Rate,"S","0"))) %>%
  # filter(Number > 5) %>%
  mutate(population = ifelse(agegrpid <= 3, "child",'adult')) %>%
  group_by(population,Year) %>%
  summarize(`Incidence number` = as.integer(sum(Number))) %>%
  pivot_wider(names_from = Year,values_from = `Incidence number`) %>%
  rempsyc::nice_table(title="Table 1: Number of New Registered Cancers")
  

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
library(sf)
incidence_sexfiltered <- read_csv("data/clean/incidence_sexfiltered.csv")
mortality_sexfiltered <- read_csv('data/clean/mortality_sexfiltered.csv')
DHB_map <- st_read("data/NZ_District_Health_Board_boundaries_-_generalised.kml", quiet=TRUE)

ggmap_incidence <- cancer_region_map(data = incidence_sexfiltered , type ="Incidence",map = DHB_map)
ggmap_incidence


ggmap_incidence <- cancer_region_map(data = mortality_sexfiltered , type ="Mortality",map = DHB_map)
ggmap_incidence

#### 7. Time changes ####

incidence_sexfiltered <- read_csv("data/clean/incidence_sexfiltered.csv")
mortality_sexfiltered <- read_csv('data/clean/mortality_sexfiltered.csv')

#label number calculated by mean(rate[2016:2020]) - mean(rate[2011:2015]), positive indicate a increasing trends, vice versa.
gg_time <- time_change_plot(data = incidence_sexfiltered, type = "Incidence",label.size = 2.5)
gg_time 



