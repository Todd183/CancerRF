
#### 1. Environment setup ####
library(tidyverse)
library(sf)
#### 2. Data Cleaning ####


#### 2.1. Cancer data ####

incidence <- read_csv('data/cancer-registrations-by-dhb.csv')
incidence %>% 
  filter(DHB != "Overseas and undefined") %>% #remove oversea data
  mutate(cancer =  str_remove(`Cancer type`,"\\s\\(.*\\)")) %>% #remove cancer ICD codes
  rename(year = Year, sex = Sex, incidence_num = Number, incidence_rate = Rate) %>%
  select (DHB, year, sex, cancer, incidence_num, incidence_rate ) %>%
  mutate(incidence_rate = str_replace(incidence_rate,"S","0")) %>%
  mutate(incidence_rate = as.numeric(incidence_rate)) ->
  incidence

write_csv(incidence,"data/clean/incidence.csv")

mortality <- read_csv('data/cancer-deaths-by-dhb.csv')
mortality %>% 
  filter(DHB != "Overseas and undefined" & `Cancer type` != "Unspecified site") %>%  #remove oversea data and undefined cancers
  mutate(cancer =  str_remove(`Cancer type`,"\\s\\(.*\\)")) %>% #remove cancer ICD codes
  rename(year = Year, sex = Sex, mortality_num = Number, mortality_rate = Rate) %>%
  select (DHB, year, sex, cancer, mortality_num, mortality_rate ) %>%
  mutate(mortality_rate = str_replace(mortality_rate,"S","0")) %>%
  mutate(mortality_rate = as.numeric(mortality_rate)) ->
  mortality

write_csv(mortality,"data/clean/mortality.csv")

#### 2.2. DHB Map data ####

DHB_map <- st_read("data/NZ_District_Health_Board_boundaries_-_generalised.kml", quiet=TRUE)
DHB_map %>% 
  mutate(DHB = case_when(DHB_name == "Capital and Coast" ~ "Capital & Coast", #modify DHB names in DHB_mapto match cancer
                         TRUE ~ DHB_name)) %>%
  select(DHB,geometry) -> 
  DHB_map

write_sf(DHB_map,"data/clean/mapdata/DHB_map.shp")
# DHB_map %>%
#   ggplot( ) +
#   geom_sf(aes(fill = DHB_name))+
#   coord_sf(xlim = c(165,179), ylim = c(-50,-30))
# 



#### 2.3. New Zealand Health Survey data ####
nzhs <- read_csv('data/nz-health-survey-2017-20-regional-update-dhb-prevalences.csv')
nzhs %>% 
  filter(!grepl("-",year) & type == "STD") %>%
  # filter(!grepl("-",year)) %>%
  mutate(DHB = case_when(region == 'Tairāwhiti' ~ "Tairawhiti", #modify DHB names in DHB_mapto match cancer
                         region == 'Waitematā' ~ 'Waitemata',
                         region == "New Zealand" ~ 'All New Zealand',
                         TRUE ~ region),
         year = as.numeric(str_extract(year,"^[0-9]*")),
         rf = short.description,
         sex = case_when(sex == 'All' ~ "AllSex",
                         TRUE ~ sex)
  ) %>% 
  select(DHB,year,population,sex,rf,Prevalence) %>%
  pivot_wider(names_from = c(population,sex,rf), names_sep = '-',values_from = Prevalence) -> nzhs_wide
 

write_csv(nzhs_wide,"data/clean/nzhs_wide.csv")


#### 2.4 Humanities data ####

#### 2.4.1 Education data ####

education <- read.csv('data/people/Statistical Area 1 dataset for Census 2018 – total New Zealand – Long format_updated_16-7-20/Individual_part2_totalNZ_updated_16-7-20/Highest_qualification_long_updated_16-7-20.csv')

qualifications_to_level <- c(
  "No qualification" = 'No qualification',
  "Level 1 certificate" = 'level 1',
  "Level 2 certificate" = 'level 2',
  "Level 3 certificate" = 'level 3',
  "Level 4 certificate" = 'level 4',
  "Level 5 diploma" = 'level 5',
  "Level 6 diploma" = 'level 6',
  "Bachelor degree and Level 7 qualification" = 'level 7',
  "Post-graduate and honours degrees" = 'level 8',
  "Masters degree" = 'level 9',
  "Doctorate degree" = 'level 10'
)

qualifications_map <- c(
  'No qualification' = "≥ level 1",
  'level 1' = "≥ level 2",
  'level 2' = "≥ level 3",
  'level 3' = "≥ level 4",
  'level 4' = "≥ level 5",
  'level 5' = "≥ level 6",
  'level 6' = "≥ level 7",
  'level 7' = "≥ level 8",
  'level 8' = "≥ level 9",
  'level 9' = "≥ level 10"
)


education %>% 
  mutate(Area_description = str_replace( Area_description,'Capital and Coast',"Capital & Coast")) %>%
  filter(Year >= 2011 & Year <= 2020 &
           grepl("DHB",Area_code_and_description) &
           !Highest_qualification_descriptor %in% c('Overseas secondary school qualification', "Not elsewhere included","Total")) %>%
  rename( DHB = Area_description, year = Year) %>%
  mutate(highest_edu =  qualifications_to_level[Highest_qualification_descriptor], Count = as.numeric(Count) ) %>%
  group_by(DHB,year) %>%
  mutate(percentage = Count / Count[Highest_qualification_descriptor == "Total stated"] * 100) %>% 
  filter(Highest_qualification_descriptor != 'Total stated') %>% 
  select(DHB,highest_edu,percentage,year) -> edu1

edu1 %>%  
  group_by(DHB,year) %>%
  mutate(percentage = 100 - cumsum(percentage)) %>% 
  filter(highest_edu != 'level 10') %>%
  mutate(highest_edu = qualifications_map[highest_edu]) -> edu2

edu <- rbind(edu1,edu2) %>% pivot_wider(names_from = highest_edu,values_from = percentage)

colnames(edu)[-c(1,2)] <- paste0('Education-',colnames(edu)[-c(1,2)] )


write_csv(edu,"data/clean/highest_qulification.csv")




#### 2.4.2 Working hours ####

work_hours <- read.csv('data/people/Statistical Area 1 dataset for Census 2018 – total New Zealand – Long format_updated_16-7-20/Individual_part3b_totalNZ_updated_16-7-20/total_hours_worked_long_updated_16-7-20.csv')

work_hours_map <- c(
  "1-9 hours worked" = "≥ 10 hours"  ,
  "10-19 hours worked" = "≥ 20 hours",
  "20-29 hours worked" = "≥ 30 hours",
  "30-39 hours worked" = "≥ 40 hours",
  "40-49 hours worked" = "≥ 50 hours",
  "50-59 hours worked" = "≥ 60 hours"
)

work_hours %>% 
  mutate(Area_description = str_replace( Area_description,'Capital and Coast',"Capital & Coast")) %>%
  filter(Year >= 2011 & Year <= 2020 &
           grepl("DHB",Area_code_and_description) &
           !Hours_worked_week_descriptor %in% c( "Not elsewhere included","Total")) %>%
  rename( DHB = Area_description, year = Year) %>%
  mutate(Count = as.numeric(Count) ) %>%
  group_by(DHB,year) %>%
  mutate(percentage = Count / Count[Hours_worked_week_descriptor == "Total stated"] * 100) %>% 
  filter(Hours_worked_week_descriptor != 'Total stated') %>% 
  select(DHB,Hours_worked_week_descriptor,percentage,year) -> whs1

whs1 %>%  
  group_by(DHB,year) %>%
  mutate(percentage = 100 - cumsum(percentage)) %>% 
  filter(Hours_worked_week_descriptor != '60 hours or more worked') %>%
  mutate(Hours_worked_week_descriptor = work_hours_map[Hours_worked_week_descriptor]) -> whs2

whs <- rbind(whs1,whs2) %>% pivot_wider(names_from = Hours_worked_week_descriptor,values_from = percentage)

colnames(whs)[-c(1,2)] <- paste0('WorkHours-',colnames(whs)[-c(1,2)] )


write_csv(whs,"data/clean/work_hours.csv")


#### 2.4.3 Income data  ####

income_cesus <- read.csv('data/people/Statistical Area 1 dataset for Census 2018 – total New Zealand – Long format_updated_16-7-20/Individual_part2_totalNZ_updated_16-7-20/Total_personal_income_long_updated_16-7-20.csv')

income_map <- c(
  "$5,000 or less" = "> $5,000",
  "$5,001-$10,000" = "> $10,000",
  "$10,001-$20,000" = "> $20,000",
  "$20,001-$30,000" = "> $30,000",
  "$30,001-$50,000" = "> $50,000",
  "$50,001-$70,000" = "> $70,000"
)

income_cesus %>% 
  mutate(Area_description = str_replace( Area_description,'Capital and Coast',"Capital & Coast")) %>%
  filter(Year >= 2011 & Year <= 2020 &
           grepl("DHB",Area_code_and_description) &
           !Grouped_personal_income_descriptor %in% c( "Not stated","Total","Median, ($)")) %>%
  rename( DHB = Area_description, year = Year) %>%
  mutate(Count = as.numeric(Count) ) %>%
  group_by(DHB,year) %>%
  mutate(percentage = Count / Count[Grouped_personal_income_descriptor == "Total stated"] * 100) %>% 
  filter(Grouped_personal_income_descriptor != 'Total stated') %>% 
  select(DHB,Grouped_personal_income_descriptor,percentage,year) -> income1


income1 %>%  
  group_by(DHB,year) %>%
  mutate(percentage = 100 - cumsum(percentage)) %>% 
  filter(Grouped_personal_income_descriptor != '$70,001 or more') %>%
  mutate(Grouped_personal_income_descriptor = income_map[Grouped_personal_income_descriptor]) -> income2


income <- rbind(income1,income2) %>% pivot_wider(names_from = Grouped_personal_income_descriptor,values_from = percentage)

colnames(income)[-c(1,2)] <- paste0('Income-',colnames(income)[-c(1,2)] )


write_csv(income,"data/clean/income.csv")


#### 2.4.4 Children born  ####

children_census <- read.csv('data/people/Statistical Area 1 dataset for Census 2018 – total New Zealand – Long format_updated_16-7-20/Individual_part2_totalNZ_updated_16-7-20/Number_of_children_born_long_updated_16-7-20.csv')

children_map <- c(
    "No children" = "> 0 children",
    "One child" = "> 1 children",
    "Two children" = "> 2 children",
    "Three children" = "> 3 children",
    "Four children" = "> 4 children",
    "Five children" = "> 5 children"
  )


children_census %>% 
  mutate(Area_description = str_replace( Area_description,'Capital and Coast',"Capital & Coast")) %>%
  filter(Year >= 2011 & Year <= 2020 &
           grepl("DHB",Area_code_and_description) &
           !Number_children_born_descriptor %in% c("Total","Not elsewhere included","Object to answering")) %>%
  rename( DHB = Area_description, year = Year) %>%
  mutate(Count = as.numeric(Count) ) %>%
  group_by(DHB,year) %>%
  mutate(percentage = Count / Count[Number_children_born_descriptor == "Total stated"] * 100) %>% 
  filter(Number_children_born_descriptor != 'Total stated') %>% 
  select(DHB,Number_children_born_descriptor,percentage,year) -> children1


children1 %>%  
  group_by(DHB,year) %>%
  mutate(percentage = 100 - cumsum(percentage)) %>% 
  filter(Number_children_born_descriptor != 'Six or more children') %>%
  mutate(Number_children_born_descriptor = children_map[Number_children_born_descriptor]) -> children2

children <- rbind(children1,children2) %>% pivot_wider(names_from = Number_children_born_descriptor,values_from = percentage)

colnames(children)[-c(1,2)] <- paste0('Children-',colnames(children)[-c(1,2)] )



write_csv(children,"data/clean/number_of_children.csv")


#### 2.5 Environment data  ####
DHB_map <- st_read("data/clean/mapdata/DHB_map.shp", quiet=TRUE)

### 2.5.1 Earthquake data ####

earthquake_2007_to_2023 <- read_csv('data/environment/earthquake2007-2023.csv')

earthquake_2007_to_2023 %>% 
  filter(eventtype == 'earthquake') %>%
  filter(origintime > as.Date("2011-01-01") &  origintime < as.Date("2020-12-31")) %>%
  mutate(year = format(origintime, "%Y"))%>%
  select(year,longitude,latitude,magnitude,depth) %>% 
  st_as_sf(.,coords = c("longitude", "latitude"), crs = 4326) %>%
  st_join(.,DHB_map) %>%
  filter(!is.na(DHB))->
  earthquake_map

earthquake_map %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(year,DHB) %>%
  summarise(magnitude_max =  max(magnitude), magnitude_mean = mean(magnitude), 
            depth_max =  max(depth), depth_mean = mean(depth), 
            counts = n()) %>%
  pivot_wider(names_from = DHB, names_sep = "-" , values_from = c(3:7), values_fill = 0) %>% #some DHB does not have earthquake, change to generate those rows with 0
  pivot_longer(names_to = c('category','DHB'), names_sep = '-', values_to = "values", cols = -1) %>% #change back to wide, but all values are in single column
  pivot_wider(names_from = category , values_from = values) -> #change values back to their original category
  earthquake

colnames(earthquake)[-c(1,2)] <- paste0('Earthquake-',colnames(earthquake)[-c(1,2)] )

write_sf(earthquake_map,"data/clean/mapdata/earthquake.shp")
write_csv(earthquake,"data/clean/earthquake.csv")


### 2.5.2 Seasonal temperature data ####
tmp_1951_to_2022 <- readxl::read_xlsx('data/environment/annual&seasonal-temperature 1951-2022.xlsx')


tmp_1951_to_2022 %>% 
  filter(year >= 2011 & year <= 2020) %>%
  select( year, site, statistic, season , temperature,lat,lon) %>% 
  st_as_sf(.,coords = c("lon", "lat"), crs = 4326) %>%
  st_join(.,DHB_map) %>%
  filter(!is.na(DHB))->
  tmp_map

tmp_map %>%
  as.data.frame() %>%
  select(-c(geometry,site)) %>%
  group_by(year,DHB,statistic,season) %>%
  summarise(temperature = mean(temperature)) %>%
  pivot_wider(names_from = c(statistic,season), names_sep = '_', values_from = temperature) ->
  tmp

colnames(tmp)[-c(1,2)] <- paste0('Temperature-',colnames(tmp)[-c(1,2)] )


write_sf(tmp_map,"data/clean/mapdata/temp_map.shp")
write_csv(tmp,"data/clean/temperature.csv")


### 2.5.3 Air quality data ####

air_2016_to_2022 <- readxl::read_xlsx('data/environment/air-quality2016-2022.xlsx')

air_2016_to_2022 %>% 
  mutate(year =  format(as.Date(`Sample Date`),'%Y'),
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  filter(year >= 2011 & year <= 2020) %>%
  select( Latitude, Longitude, year, Indicator,Concentration) %>% 
  st_as_sf(.,coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(.,DHB_map) %>%
  filter(!is.na(DHB))->
  air_map

air_map %>%
  as.data.frame() %>%
  select(-c(geometry)) %>%
  group_by(year,DHB,Indicator) %>%
  summarise(concentration_max = max(Concentration,na.rm=T),
            concentration_mean = mean(Concentration,na.rm=T)) %>%
  pivot_wider(names_from = c(Indicator), names_sep = '_', values_from = c(concentration_max,concentration_mean)) ->
  air # there are some missing values

colnames(air)[-c(1,2)] <- paste0('Air-',colnames(air)[-c(1,2)] )

write_sf(air_map,"data/clean/mapdata/air_map.shp")
write_csv(air,"data/clean/air.csv")

### 2.5.4 ground water quality  ####

water_2014_to_2021 <- readxl::read_xlsx('data/environment/ground water qualitiy 2014-2021.xlsx')

water_2014_to_2021 %>% 
  mutate(year =  format(as.Date(Date),'%Y')) %>%
  filter(year >= 2011 & year <= 2020) %>%
  select( Latitude, Longitude, year, Indicator,CensoredValue) %>% 
  st_as_sf(.,coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(.,DHB_map) %>%
  filter(!is.na(DHB)) ->
  water_map

water_map %>%
  as.data.frame() %>%
  mutate(Indicator = str_replace(Indicator,"E\\. coli","E\\. Coli")) %>%
  select(-c(geometry)) %>%
  group_by(year,DHB,Indicator) %>%
  summarise(censoredValue_max = max(CensoredValue,na.rm=T),
            censoredValue_mean = mean(CensoredValue,na.rm=T)) %>%
  pivot_wider(names_from = c(Indicator), names_sep = '_', values_from = c(censoredValue_max,censoredValue_mean)) ->
  water # there are some missing values

colnames(water)[-c(1,2)] <- paste0('Water-',colnames(water)[-c(1,2)] )


write_sf(water_map,"data/clean/mapdata/water_map.shp")
write_csv(water,"data/clean/water.csv")




#### 2.6 combine all data ####

water = read_csv("data/clean/water.csv")
air = read_csv("data/clean/air.csv")
tmp = read_csv("data/clean/temperature.csv")
earthquake = read_csv("data/clean/earthquake.csv")
income = read_csv("data/clean/income.csv")
education = read_csv("data/clean/highest_qulification.csv")
children = read_csv("data/clean/number_of_children.csv")
workhours = read_csv("data/clean/work_hours.csv")
nzhs =  read_csv("data/clean/nzhs_wide.csv")

incidence = read_csv("data/clean/incidence.csv")
mortality = read_csv("data/clean/mortality.csv")

rf <- list("water" = water, 
           "air" = air,
           "tmp" = tmp,
           "earthquake" = earthquake,
           "income" = income,
           "education" = education,
           "children" = children,
           "workhours" = workhours,
           "nzhs" = nzhs)

#modified incidence rate and mortality rate based on sex distribution of cancer tyeps

#filter incidence based on sex
cancer_sex <- incidence %>%
  filter(DHB == "All New Zealand") %>%
  group_by(year,cancer,sex) %>%
  summarise(rate_mean = mean(incidence_rate,na.rm=T)) %>%
  pivot_wider(names_from = sex, values_from = rate_mean ) %>%
  mutate(group = case_when(is.na(Male) & !is.na(Female) ~ "Female",
                           !is.na(Male) & is.na(Female) ~ "Male",
                           TRUE ~ "AllSex")) %>%
  ungroup(.) %>%
  select(cancer,group) %>%
  filter(!duplicated(cancer))

incidence %>% 
  left_join(.,cancer_sex) %>%
  filter(sex == group) -> incidence

#filter mortality based on sex
cancer_sex <- mortality %>%
  filter(DHB == "All New Zealand") %>%
  group_by(year,cancer,sex) %>%
  summarise(rate_mean = mean(mortality_rate,na.rm=T)) %>%
  pivot_wider(names_from = sex, values_from = rate_mean ) %>%
  mutate(group = case_when(is.na(Male) & !is.na(Female) ~ "Female",
                           !is.na(Male) & is.na(Female) ~ "Male",
                           TRUE ~ "AllSex")) %>%
  ungroup(.) %>%
  select(cancer,group) %>%
  filter(!duplicated(cancer))

mortality %>% 
  left_join(.,cancer_sex) %>%
  filter(sex == group) -> mortality





# Split the data frame by Col1

incidence %>% 
  filter(DHB != "All New Zealand") %>%
  split(., .[,"cancer"]) %>% #split by cancer
    lapply(., function(df){
      ls = split(df, df$sex) #split by sex
      lapply(ls, function(sub_df){
        sub_df = sub_df[, c('year','DHB',"incidence_rate")]
        sub_ls = lapply(rf,inner_join, x=sub_df, by = c("DHB","year")) #join rf and incidence
        names(sub_ls) = names(rf)
        return(sub_ls)
        })
      } ) -> incidence_ls


mortality %>% 
  filter(DHB != "All New Zealand") %>%
  split(., .[,"cancer"]) %>% #split by cancer
  lapply(., function(df){
    ls = split(df, df$sex) #split by sex
    lapply(ls, function(sub_df){
      sub_df = sub_df[, c('year','DHB',"mortality_rate")]
      sub_ls = lapply(rf,inner_join, x=sub_df, by = c("DHB","year")) #join rf and mortality
      names(sub_ls) = names(rf)
      return(sub_ls)
    })
  } ) -> mortality_ls

save(incidence_ls,file = 'data/clean/incidence_rf.Rdata')
save(mortality_ls, file = 'data/clean/mortality_rf.Rdata')


