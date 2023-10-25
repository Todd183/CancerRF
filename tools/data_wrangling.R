#### New Zealand Health Survey data ####
nzhs <- read_csv('data/raw/nz-health-survey-2017-20-regional-update-dhb-prevalences.csv')
nzhs %>% 
  filter(population == "adults") %>% #select adults population group for analysis
  filter(!grepl("-",year) & type == "STD") %>% #filter age-standardized data with type == "STD", remove combined year data.
  mutate(DHB = case_when(region == 'Tairāwhiti' ~ "Tairawhiti", #modify DHB names in DHB_mapto match cancer
                         region == 'Waitematā' ~ 'Waitemata',
                         region == "New Zealand" ~ 'All New Zealand',
                         TRUE ~ region),
         year = as.numeric(str_extract(year,"^[0-9]*")), #just keep the year information
         rf = short.description,
         sex = case_when(sex == 'All' ~ "AllSex",
                         TRUE ~ sex),
         value = Prevalence,
         type = "percentage"
  ) %>% 
  select(DHB,year,sex,rf,value,type) %>%
  mutate(category = "NZHS", .before = 4) ->
  # pivot_wider(names_from = c(rf), names_sep = '-',values_from = Prevalence) ->
  nzhs_long # Note there are some missing values after changing to wide data format


#### Education data ####

education <- read.csv('data/raw/Highest_qualification_long_updated_16-7-20.csv')

#clean the Education data and map to qualification levels
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

#create new variables with the "≥ levels" format
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
  mutate(Area_description = str_replace( Area_description,'Capital and Coast',"Capital & Coast")) %>% #unify DHB names
  filter(Year >= 2011 & Year <= 2020 & #screen years match the cancer data
           grepl("DHB",Area_code_and_description) & #filter rows with DHB information
           !Highest_qualification_descriptor %in% c('Overseas secondary school qualification', "Not elsewhere included","Total")) %>% #keep only NZ education qualication data
  rename( DHB = Area_description, year = Year) %>%
  mutate(rf =  qualifications_to_level[Highest_qualification_descriptor], Count = as.numeric(Count) ) %>%
  group_by(DHB,year) %>%
  mutate(value = Count / Count[Highest_qualification_descriptor == "Total stated"] * 100,
         type = "percentage") %>% #calculate the percentage of each qualification level
  filter(Highest_qualification_descriptor != 'Total stated') %>% 
  select(DHB,year,rf,value,type) %>%
  mutate(sex = "AllSex", category = 'Education' , .before =3)-> edu1

edu1 %>%  
  group_by(DHB,year) %>%
  mutate(value = 100 - cumsum(value)) %>% #calculate the cumulative percentage of each "≥ level" variable
  filter(rf != 'level 10') %>% #remove the last variable which has the 100% percentage 
  mutate(rf = qualifications_map[rf]) -> edu2

edu <- rbind(edu1,edu2) 

#### Work hours data ####


#### Working hours ####

work_hours <- read.csv('data/raw/total_hours_worked_long_updated_16-7-20.csv')

#create new variables with the "≥ hours" format
work_hours_map <- c(
  "1-9 hours worked" = "≥ 10 hours"  ,
  "10-19 hours worked" = "≥ 20 hours",
  "20-29 hours worked" = "≥ 30 hours",
  "30-39 hours worked" = "≥ 40 hours",
  "40-49 hours worked" = "≥ 50 hours",
  "50-59 hours worked" = "≥ 60 hours"
)

work_hours %>% 
  mutate(Area_description = str_replace( Area_description,'Capital and Coast',"Capital & Coast")) %>% #unify DHB names
  filter(Year >= 2011 & Year <= 2020 &  #screen years match the cancer data
           grepl("DHB",Area_code_and_description) & #filter rows with DHB information
           !Hours_worked_week_descriptor %in% c( "Not elsewhere included","Total")) %>%
  rename( DHB = Area_description, year = Year) %>%
  mutate(Count = as.numeric(Count) ) %>%
  group_by(DHB,year) %>%
  mutate(value = Count / Count[Hours_worked_week_descriptor == "Total stated"] * 100,  #calculate the percentage of each work hours length
         rf = Hours_worked_week_descriptor,
         type = "percentage") %>%
  filter(Hours_worked_week_descriptor != 'Total stated') %>% 
  select(DHB,year,rf,value,type) %>%
  mutate(sex = "AllSex", category = 'Work Hours' , .before =3) -> whs1

whs1 %>%  
  group_by(DHB,year) %>%
  mutate(value = 100 - cumsum(value)) %>% #calculate the cumulative percentage of each "≥ hours" variable
  filter(rf != '60 hours or more worked') %>% #remove the last variable which has the 100% percentage 
  mutate(rf = work_hours_map[rf]) -> whs2

whs <- rbind(whs1,whs2) 


#### Income data  ####

income_cesus <- read.csv('data/raw/Total_personal_income_long_updated_16-7-20.csv')


#create new variables with the "≥ $" format
income_map <- c(
  "$5,000 or less" = "> $5,000",
  "$5,001-$10,000" = "> $10,000",
  "$10,001-$20,000" = "> $20,000",
  "$20,001-$30,000" = "> $30,000",
  "$30,001-$50,000" = "> $50,000",
  "$50,001-$70,000" = "> $70,000"
)

income_cesus %>% 
  mutate(Area_description = str_replace( Area_description,'Capital and Coast',"Capital & Coast")) %>% #unify DHB names
  filter(Year >= 2011 & Year <= 2020 &  #screen years match the cancer data
           grepl("DHB",Area_code_and_description) & #filter rows with DHB information
           !Grouped_personal_income_descriptor %in% c( "Not stated","Total","Median, ($)")) %>%
  rename( DHB = Area_description, year = Year) %>%
  mutate(Count = as.numeric(Count) ) %>%
  group_by(DHB,year) %>%
  mutate(value = Count / Count[Grouped_personal_income_descriptor == "Total stated"] * 100,
         rf = Grouped_personal_income_descriptor,
         type = "percentage") %>% #calculate the percentage of each income range
  filter(Grouped_personal_income_descriptor != 'Total stated') %>% 
  select(DHB,year,rf,value,type)  %>%
  mutate(sex = "AllSex", category = 'Income' , .before =3)-> income1


income1 %>%  
  group_by(DHB,year) %>%
  mutate(value = 100 - cumsum(value)) %>% #calculate the cumulative percentage of each "≥ $" variable
  filter(rf != '$70,001 or more') %>% #remove the last variable which has the 100% percentage 
  mutate(rf = income_map[rf]) -> income2


income <- rbind(income1,income2) 


#### Birth numbers ####

children_census <- read.csv('data/raw/Number_of_children_born_long_updated_16-7-20.csv')

#create new variables with the "> children" format
children_map <- c(
  "No children" = "> 0 children",
  "One child" = "> 1 children",
  "Two children" = "> 2 children",
  "Three children" = "> 3 children",
  "Four children" = "> 4 children",
  "Five children" = "> 5 children"
)


children_census %>% 
  mutate(Area_description = str_replace( Area_description,'Capital and Coast',"Capital & Coast")) %>% #unify DHB names
  filter(Year >= 2011 & Year <= 2020 & #screen years match the cancer data
           grepl("DHB",Area_code_and_description) & #filter rows with DHB information
           !Number_children_born_descriptor %in% c("Total","Not elsewhere included","Object to answering")) %>%
  rename( DHB = Area_description, year = Year) %>%
  mutate(Count = as.numeric(Count) ) %>%
  group_by(DHB,year) %>%
  mutate(value = Count / Count[Number_children_born_descriptor == "Total stated"] * 100,
         rf = Number_children_born_descriptor,
         type = "percentage") %>%  #calculate the percentage 
  filter(Number_children_born_descriptor != 'Total stated') %>% 
  select(DHB,year,rf,value,type) %>%
  mutate(sex = "AllSex", category = 'Birth Number' , .before =3) -> children1


children1 %>%  
  group_by(DHB,year) %>%
  mutate(value = 100 - cumsum(value)) %>% #calculate the cumulative percentage of each "> children" variable
  filter(rf != 'Six or more children') %>%  #remove the last variable which has the 100% percentage 
  mutate(rf = children_map[rf]) -> children2

children <- rbind(children1,children2) 


#Map data
library(sf)
library(tidyverse)
DHB_map <- st_read("data/raw/NZ_District_Health_Board_boundaries_-_generalised.kml", quiet=TRUE)
DHB_map %>% 
  mutate(DHB = case_when(DHB_name == "Capital and Coast" ~ "Capital & Coast", #modify DHB names in DHB_mapto match cancer
                         TRUE ~ DHB_name)) %>%
  select(DHB,geometry) -> 
  DHB_map


### Earthquake data ####

earthquake_2007_to_2023 <- read_csv('data/raw/earthquake2007-2023.csv') #load earthquake data

earthquake_2007_to_2023 %>% 
  filter(eventtype == 'earthquake') %>%
  filter(origintime > as.Date("2011-01-01") &  origintime < as.Date("2020-12-31")) %>% #filter year matching cancer data
  mutate(year = format(origintime, "%Y"))%>% #extrat year information 
  select(year,longitude,latitude,magnitude,depth) %>% 
  st_as_sf(.,coords = c("longitude", "latitude"), crs = 4326) %>% #transform into sf object for coordinates mapping
  st_join(.,DHB_map) %>% # coordinates mapping
  filter(!is.na(DHB))-> #remove coordinates that failed to map to DHB region
  earthquake_map

earthquake_map %>%
  as.data.frame() %>%
  select(-geometry) %>%
  group_by(year,DHB) %>% #summarize based on the year and DHB group
  summarise(magnitude_max =  max(magnitude), magnitude_mean = mean(magnitude), 
            depth_max =  max(depth), depth_mean = mean(depth), 
            counts = n()) %>%
  pivot_wider(names_from = DHB, names_sep = "-" , values_from = c(3:7), values_fill = 0) %>% #some DHB does not have earthquake, change to generate those rows with 0
  pivot_longer(names_to = c('rf','DHB'), names_sep = '-', values_to = "value", cols = -1) %>% #change back to wide, but all values are in single column
  select(DHB,year,rf,value) %>%
  mutate(sex = "AllSex", category = 'Earthquake' , .before =3) %>%
  mutate(type = "value")-> #change values back to their original category
  earthquake



### Temperature data ####
#load temperature data with read_xlsx and suppress warnings messages
suppressWarnings(tmp_1951_to_2022 <- readxl::read_xlsx('data/raw/annual&seasonal-temperature 1951-2022.xlsx'))


tmp_1951_to_2022 %>% 
  filter(year >= 2011 & year <= 2020) %>% #filter year matching cancer data
  select( year, site, statistic, season , temperature,lat,lon) %>% 
  st_as_sf(.,coords = c("lon", "lat"), crs = 4326) %>% #transform into sf object for coordinates mapping
  st_join(.,DHB_map) %>% # coordinates mapping
  filter(!is.na(DHB))-> #remove coordinates that failed to map to DHB region
  tmp_map

tmp_map %>%
  as.data.frame() %>%
  select(-c(geometry,site)) %>%
  group_by(year,DHB,statistic,season) %>% #summarize based on the year and DHB group
  summarise(temperature = mean(temperature)) %>%
  ungroup(.) %>%
  pivot_wider(names_from = c(statistic,season), names_sep = '_', values_from = temperature) %>%
  pivot_longer(names_to = c('rf'),  values_to = "value", cols = -c(1:2)) %>%
  select(DHB,year,rf,value) %>%
  mutate(sex = "AllSex", category = 'Temperature' , .before =3) %>%
  mutate(type = "value")->
  tmp


### Air quality data ####
air_2016_to_2022 <- readxl::read_xlsx('data/raw/air-quality2016-2022.xlsx')

air_2016_to_2022 %>% 
  mutate(year =  format(as.Date(`Sample Date`),'%Y'), #extrate year info
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  filter(year >= 2011 & year <= 2020) %>% #filter year matching cancer data
  select( Latitude, Longitude, year, Indicator,Concentration) %>% 
  st_as_sf(.,coords = c("Longitude", "Latitude"), crs = 4326) %>% #transform into sf object for coordinates mapping
  st_join(.,DHB_map) %>%  # coordinates mapping
  filter(!is.na(DHB))-> #remove coordinates that failed to map to DHB region
  air_map

air_map %>%
  as.data.frame() %>%
  select(-c(geometry)) %>%
  group_by(year,DHB,Indicator) %>%  #summarize based on the year and DHB group
  summarise(concentration_max = max(Concentration,na.rm=T),
            concentration_mean = mean(Concentration,na.rm=T)) %>%
  pivot_longer(names_to = c('rf'),  values_to = "value", cols = -c(1:3)) %>%
  mutate(rf = paste0(Indicator,'_',rf)) %>%
  select(DHB,year,rf,value) %>%
  mutate(sex = "AllSex", category = 'Air quality' , .before =3) %>%
  mutate(type = "value") -> 
  air # there are some missing values


#### Ground water quality ####

suppressWarnings(water_2014_to_2021 <- readxl::read_xlsx('data/raw/ground water qualitiy 2014-2021.xlsx'))

water_2014_to_2021 %>% 
  mutate(year =  format(as.Date(Date),'%Y')) %>% #extrate year info
  filter(year >= 2011 & year <= 2020) %>% #filter year matching cancer data
  select( Latitude, Longitude, year, Indicator,CensoredValue) %>% 
  st_as_sf(.,coords = c("Longitude", "Latitude"), crs = 4326) %>% #transform into sf object for coordinates mapping
  st_join(.,DHB_map) %>% # coordinates mapping
  filter(!is.na(DHB)) -> #remove coordinates that failed to map to DHB region
  water_map

water_map %>%
  as.data.frame() %>%
  mutate(Indicator = str_replace(Indicator,"E\\. coli","E\\. Coli")) %>%
  select(-c(geometry)) %>%
  group_by(year,DHB,Indicator) %>% #summarize based on the year and DHB group
  summarise(censoredValue_max = max(CensoredValue,na.rm=T),
            censoredValue_mean = mean(CensoredValue,na.rm=T)) %>%
  pivot_longer(names_to = c('rf'),  values_to = "value", cols = -c(1:3)) %>%
  mutate(rf = paste0(Indicator,'_', str_remove(rf,'censoredValue_'))) %>%
  select(DHB,year,rf,value) %>%
  mutate(sex = "AllSex", category = 'Water quality' , .before =3) %>%
  mutate(type = "value") -> water


## combine all rf datasets


rf <- reduce(list(nzhs_long,whs,children,income,edu,earthquake,tmp,water,air),rbind) %>%
  mutate(year = as.numeric(year))

rf %>% skimr::skim()

rf %>% mutate(checked = duplicated(paste(DHB,year,sex,category,rf)))  %>% count(checked)

save(rf,file = 'data/clean/rf.Rdata')









