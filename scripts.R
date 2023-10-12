
#### 1. Environment setup ####
library(tidyverse)
library(ggplot2)
library(skimr)
library(ComplexHeatmap)
library(circlize)
library(ggsci)
library(lubridate)
library(flextable)
library(rempsyc)
library(sf)
library(spData)
library(rgeos)
library(ggrepel)

#### 2. Loading Data ####

cancer <- read_csv('data/cancer-registrations-by-dhb.csv')
DHB_map <- st_read("data/NZ_District_Health_Board_boundaries_-_generalised.kml", quiet=TRUE)


DHB_TO_ABBREVIATION <- c(
  "Auckland" = "AUK",
  "Bay of Plenty" = "BOP",
  "Canterbury" = "CAN",
  "Capital & Coast" = "CC",
  "Counties Manukau" = "CM",
  # "Gisborne" = "GIS",
  "Hawke's Bay" = "HB",
  "Hutt Valley" = "HV",
  "Lakes" = "Lakes",
  "MidCentral" = "MD",
  "Nelson Marlborough" = "NM",
  # "Manawatu-Wanganui" = "MWT",
  # "Marlborough" = "MBH",
  # "Nelson" = "NSN",
  "Northland" = "NTL",
  "South Canterbury" = "",
  "Southern" = "",
  "Tairawhiti" = "",
  
  # "Otago" = "OTA",
  # "Southland" = "STL",
  "Taranaki" = "TKI",
  # "Tasman" = "TAS",
  "Waikato" = "WKO",
  "Wairarapa" = "",
  "Waitemata" = "",
  "Whanganui" = "",
  # "Wellington" = "WGN",
  "West Coast" = "WTC"
)


#### 1. unify DHB names ####


#modify DHB names in DHB_mapto match cancer
DHB_map %>% 
  mutate(DHB = case_when(DHB_name == "Capital and Coast" ~ "Capital & Coast",
                         TRUE ~ DHB_name)) -> DHB_map
#Verify that DHB_map and cancer share the same DHB name
any(DHB_map$DHB %in% unique(cancer$DHB))










