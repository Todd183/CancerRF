#### 1. Environment setup ####
library(tidyverse)
library(ggsci)
library(ggpubr)
library(ggrepel)
library(sf)
source('tools.R')
#### 2. Load data #### 
age <- read_csv('data/cancer-deaths-by-age.csv')


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
library(sf)
incidence_sexfiltered <- read_csv("data/clean/incidence_sexfiltered.csv")
mortality_sexfiltered <- read_csv('data/clean/mortality_sexfiltered.csv')
DHB_map <- st_read("data/NZ_District_Health_Board_boundaries_-_generalised.kml", quiet=TRUE)

ggmap_incidence <- cancer_region_map(data = incidence_sexfiltered , type ="Incidence",map = DHB_map)
ggmap_incidence


ggmap_incidence <- cancer_region_map(data = mortality_sexfiltered , type ="Mortality",map = DHB_map)
ggmap_incidence

#### 7. Cancer across time ###

incidence_sexfiltered <- read_csv("data/clean/incidence_sexfiltered.csv")
mortality_sexfiltered <- read_csv('data/clean/mortality_sexfiltered.csv')

cols16 = pal_simpsons("springfield")(16)

incidence_sexfiltered %>%
  mutate(cancer = str_replace(cancer,'Non-Hodgkin lymphoma','NHL')) %>%
  # group_by(cancer,DHB) %>%
  # summarise(rate_sd = sd(incidence_rate)) %>%
  filter(DHB == "All New Zealand") %>%
  ggplot(aes(x = cancer, y = incidence_rate,color = cancer,fill=cancer)) +
    # geom_hline(aes(yintercept = mean(incidence_rate)),color = 'grey',linetype='dashed') +
    scale_color_manual(values=cols14) +
    scale_fill_manual(values=alpha(cols16, 0.5)) +
    #geom_bar(aes(fill = group),stat = "identity") +
    #scale_fill_manual(values=cols16) +
    geom_point(shape = 21) +
    geom_line() +
    # scale_y_continuous(trans = "log10",breaks = c(0, 50, 80, 110)) + 
    scale_x_discrete(name ="Months", 
                     limits=seq(2,12,2))+
    theme_classic(base_family = "serif") +
    xlab('Months') +
    # facet_wrap(.~DHB,scales = "free") +
    theme(
      strip.background = element_rect(fill = alpha('grey',0.3),color = "white"),
      strip.text = element_text(size = 10),
      legend.position = 'top'
    ) -> gg


incidence_sexfiltered %>%
  mutate(cancer = str_replace(cancer,'Non-Hodgkin lymphoma','NHL')) %>%
  group_by(cancer,DHB) %>%
  summarise(rate_sd = sd(incidence_rate)) %>%
  # filter(DHB == "All New Zealand") %>%
  ggplot(aes(x = cancer, y = rate_sd,color = cancer,fill=cancer)) +
  geom_bar(stat='identity')+
  facet_wrap(.~DHB,scales = "free") 

incidence_sexfiltered %>%
  mutate(cancer = str_replace(cancer,'Non-Hodgkin lymphoma','NHL')) %>%
  group_by(cancer,DHB) %>%
  summarise(rate_sd = sd(incidence_rate)) %>%
  pivot_wider(names_from = cancer,values_from = rate_sd) %>% as.data.frame() -> hdata

rownames(hdata) = hdata$DHB
mat = as.matrix(hdata[,-1])

Heatmap(mat)
