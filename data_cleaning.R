
#### 1. Environment setup ####
library(tidyverse)


#### 2. Loading Data ####

cancer <- read_csv('data/cancer-registrations-by-dhb.csv')
DHB_map <- st_read("data/NZ_District_Health_Board_boundaries_-_generalised.kml", quiet=TRUE)



#### 1. unify DHB names ####

#modify DHB names in DHB_mapto match cancer
DHB_map %>% 
  mutate(DHB = case_when(DHB_name == "Capital and Coast" ~ "Capital & Coast",
                         TRUE ~ DHB_name)) -> DHB_map
#Verify that DHB_map and cancer share the same DHB name
any(DHB_map$DHB %in% unique(cancer$DHB))













