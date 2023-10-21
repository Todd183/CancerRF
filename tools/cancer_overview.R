#### 1.Environment setup ####
# Load necessary packages for data manipulation, visualization, and processing
library(tidyverse)
library(ggsci)
library(ggpubr)
library(ggrepel)
library(sf)
library(flextable)
source('tools/myfuns.R')

#### 2. Load data #### 
# Read the CSV file containing cancer deaths by age data
age <- read_csv('data/raw/cancer-registrations-by-age.csv')

#### 3. Age distribution ####
# Read the CSV file containing cancer incidence data
incidence <- read_csv("data/clean/incidence.csv")

# Process the age data:
# - Replace 'S' in the Rate column with '0' and convert it to numeric
# - Categorize age groups into 'child' and 'adult'
# - Group by 'population' and 'Year' columns
# - Sum up the number of incidences for each group
# - Reshape the data to have a wider format
# - Display the data in a table format

age %>% 
  mutate(Rate = as.numeric(str_replace(Rate,"S","0"))) %>%
  # filter(Number > 5) %>%
  mutate(population = ifelse(agegrpid <= 3, "child",'adult')) %>%
  group_by(population,Year) %>%
  summarize(`Incidence number` = as.integer(sum(Number))) %>%
  mutate(Year = paste(' ',Year," ")) %>%
  pivot_wider(names_from = Year,values_from = `Incidence number`) %>%
  rempsyc::nice_table() %>% 
  padding(padding.right = 20, part = "all") -> tab1
  

#### 4. Gender variance of cancer types ####
# Read incidence and mortality data
incidence <- read_csv("data/clean/incidence.csv")
mortality <- read_csv('data/clean/mortality.csv')

# Use the custom function 'draw_sex_variance' to create plots showing gender variance for incidence and mortality
g1 =  draw_sex_variance(incidence,type="Incidence",fulltitle=FALSE)
g2 = draw_sex_variance(mortality,type="Mortality",fulltitle=FALSE)

# Arrange the two plots side by side
# ggarrange(g1,g2)

#### 5. Regional distribution  #####
# Read incidence and mortality data filtered by sex
incidence_sexfiltered <- read_csv("data/clean/incidence_sexfiltered.csv")
mortality_sexfiltered <- read_csv('data/clean/mortality_sexfiltered.csv')

# Use the custom function 'regional_heatmap' to create heatmaps for incidence and mortality
ht_incidence = regional_heatmap(incidence_sexfiltered,type='Incidence')
# draw(ht_incidence, heatmap_legend_side = "right",annotation_legend_side="right",legend_grouping = "original")

ht_mortality = regional_heatmap(mortality_sexfiltered,type='mortality')
# draw(ht_mortality, heatmap_legend_side = "right",annotation_legend_side="right",legend_grouping = "original")


#### 6. Most common region for each cancer ####
# Read the map data of New Zealand District Health Board boundaries
library(sf)
incidence_sexfiltered <- read_csv("data/clean/incidence_sexfiltered.csv")
mortality_sexfiltered <- read_csv('data/clean/mortality_sexfiltered.csv')
DHB_map <- st_read("data/raw/NZ_District_Health_Board_boundaries_-_generalised.kml", quiet=TRUE)

# Use the custom function 'cancer_region_map' to create maps showing the most common regions for each cancer type for incidence and mortality
ggmap_incidence <- cancer_region_map(data = incidence_sexfiltered , type ="Incidence",map = DHB_map)
# ggmap_incidence

ggmap_mortality <- cancer_region_map(data = mortality_sexfiltered , type ="Mortality",map = DHB_map)
# ggmap_mortality

#### 7. Time changes ####
# Read incidence and mortality data filtered by sex
incidence_sexfiltered <- read_csv("data/clean/incidence_sexfiltered.csv")
mortality_sexfiltered <- read_csv('data/clean/mortality_sexfiltered.csv')

# Use the custom function 'time_change_plot' to create a plot showing the change in cancer rates over time
# The label number is calculated as the difference between the mean rates of 2016-2020 and 2011-2015
gg_time <- time_change_plot(data = incidence_sexfiltered, type = "Incidence",label.size = 3.5)
# gg_time 



