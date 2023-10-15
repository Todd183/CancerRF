library(tidyverse)
library(magrittr)

options(digits = 6) 
setwd('D:\\data422')
load(file = 'data/clean/incidence_rf.Rdata')
load(file = 'data/clean/mortality_rf.Rdata')

# The program will write data to csv & Rdata if WRITE_FILE is Ture
WRITE_FILES = F  # T F

# calculate p value of one feature
get_pvalue <- function(measure,df,value) {
  anova_model <- aov(as.formula(paste0(measure, "~","`",value,"`")), data=df)
  anova_summary <- summary(anova_model)
  p_value <- anova_summary[[1]][paste0("`",value,"`"), "Pr(>F)"]
  #non_NA_rows <- sum(complete.cases(df[, c(measure, value)])) %>% as.integer()
  return(p_value)
}

# calculate p value of features of particular `cancer` in `df`. 
# It will return a new dataframe, whose columns are p_value, p_adjusetd, comparisons, cancer, feature.
get_pvalue_df <- function(df,cancer,rate='incidence_rate'){
  p_values_for_feature <- data.frame(p_value=numeric(),p_adjusted=numeric(),comparisons=integer(),
                         cancer=character(),feature=character())
  for(value in names(df)[4:length(df)]){
    p_value <- get_pvalue(rate,df,value)
    p_values_for_feature %<>% rbind(setNames(c(p_value,NA,NA,cancer,value) %>% as.list(),names(p_values_for_feature)))
  }
  p_values_for_feature$comparisons <- nrow(p_values_for_feature)
  p_values_for_feature$p_adjusted <- p.adjust(p_values_for_feature$p_value)
  return(p_values_for_feature)
}


p_values_incidence <- data.frame(p_value=numeric(),p_adjusted=numeric(),comparisons=integer(),
                          cancer=character(),feature=character())
for(cancer in names(incidence_rf)){
  for(feature in names(incidence_rf[[cancer]][[1]])){
    p_values_for_feature <- get_pvalue_df(incidence_rf[[cancer]][[1]][[feature]],cancer)
    p_values_incidence %<>% rbind(p_values_for_feature)
  }
}


p_values_mortality <- data.frame(p_value=numeric(),p_adjusted=numeric(),comparisons=integer(),
                                 cancer=character(),feature=character())
for(cancer in names(mortality_rf)){
  for(feature in names(mortality_rf[[cancer]][[1]])){
    p_values_for_feature <- get_pvalue_df(mortality_rf[[cancer]][[1]][[feature]]
                                          ,cancer,rate = "mortality_rate ")
    p_values_mortality %<>% rbind(p_values_for_feature)
  }
}

p_values_incidence$p_value %<>% as.numeric()
p_values_mortality$p_value %<>% as.numeric()

#p_values_incidence %<>% arrange(p_adjusted) 
#p_values_mortality %<>% arrange(p_adjusted) 

#p_values_incidence %<>% filter(p_adjusted <= 0.9)
#p_values_mortality %<>% filter(p_adjusted <= 0.9)

if(WRITE_FILES){
  save(p_values_incidence,file = 'data/correlation_result/p_values_incidence.Rdata')
  save(p_values_mortality, file = 'data/correlation_result/p_values_mortality.Rdata')
}







cor_incidence <- data.frame(p_pearson=numeric(),cor=numeric(),cancer=character(),
                            feature=character(),comparisons_across_feature=integer())
for(cancer in names(incidence_rf)){
  comparisons_across_feature <- 0
  for(feature in names(incidence_rf[[cancer]][[1]])){
    temp <- incidence_rf[[cancer]][[1]][[feature]]
    # Only use number of children in female cancer
    if(names(incidence_rf[[cancer]]) %in% c('Male','AllSex') & feature == "children") {
      next
    }
    for(value in names(temp)[5:length(temp)]){
      cor <- cor.test(temp[[value]],temp[['incidence_rate']])$estimate  
      p_pearson <- cor.test(temp[[value]],temp[['incidence_rate']])$p.value
      cor_incidence %<>% rbind(setNames(c(p_pearson,cor,cancer,value,-1) %>% as.list(),names(cor_incidence)))
      comparisons_across_feature <- comparisons_across_feature + 1
      }
  }
  cor_incidence$comparisons_across_feature[cor_incidence$cancer == cancer] <- comparisons_across_feature
}
cor_incidence %<>% left_join(p_values_incidence, by = c('cancer','feature'))


cor_mortality <-data.frame(p_pearson=numeric(),cor=numeric(),cancer=character(),
                           feature=character(),comparisons_across_feature=integer())
for(cancer in names(mortality_rf)){
  comparisons_across_feature <- 0
  for(feature in names(mortality_rf[[cancer]][[1]])){
    temp <- mortality_rf[[cancer]][[1]][[feature]]
    # Only use number of children in female cancer
    if(names(mortality_rf[[cancer]]) %in% c('Male','AllSex') & feature == "children") {
      next
    }
    for(value in names(temp)[5:length(temp)]){
      cor <- cor.test(temp[[value]],temp[['mortality_rate']])$estimate  
      p_pearson <- cor.test(temp[[value]],temp[['mortality_rate']])$p.value
      cor_mortality %<>% rbind(setNames(c(p_pearson,cor,cancer,value,-1) %>% as.list(),names(cor_mortality)))
      comparisons_across_feature <- comparisons_across_feature + 1 
      }
  }
  cor_mortality$comparisons_across_feature[cor_mortality$cancer == cancer] <- comparisons_across_feature
}
cor_mortality %<>% left_join(p_values_mortality, by = c('cancer','feature'))
rm(temp)
gc()

cor_incidence$cor %<>% as.numeric()
cor_mortality$cor %<>% as.numeric()

for (cancer in cor_mortality$cancer %>% unique()) {
  cor_mortality$p_adjusted_all_features[cor_mortality$cancer == cancer] <- p.adjust(
    cor_mortality$p_value[cor_mortality$cancer == cancer] )
}
for (cancer in cor_incidence$cancer %>% unique()) {
  cor_incidence$p_adjusted_all_features[cor_incidence$cancer == cancer] <- p.adjust(
    cor_incidence$p_value[cor_incidence$cancer == cancer] )
}

if(WRITE_FILES){
  write.csv(cor_mortality,'data/correlation_result/cor_mortality.csv',row.names=F)
  write.csv(cor_incidence,'data/correlation_result/cor_incidence.csv',row.names=F)
}

significant_incidence <- cor_incidence %>% 
  filter(p_adjusted <= 0.05, abs(cor) >= 0.2) %>% 
  arrange(p_adjusted) 
significant_mortality <- cor_mortality %>% 
  filter(p_adjusted <= 0.05, abs(cor) >= 0.2)%>% 
  arrange(p_adjusted)
write.csv(significant_incidence,'data/correlation_result/significant_incidence.csv',row.names=F)
write.csv(significant_mortality,'data/correlation_result/significant_mortality.csv',row.names=F)



#######################################################
#  Analysis in number of children and female cancer.  #
#######################################################

setwd('C:\\Users\\niels\\OneDrive - University of Canterbury\\uc\\data422')
incidence <- read_csv('cancer-registrations-by-dhb.csv')
incidence %>% 
  filter(DHB != "Overseas and undefined") %>% #remove oversea data
  mutate(cancer =  str_remove(`Cancer type`,"\\s\\(.*\\)")) %>% #remove cancer ICD codes
  rename(year = Year, sex = Sex, incidence_num = Number, incidence_rate = Rate) %>%
  select (DHB, year, sex, cancer, incidence_num, incidence_rate ) %>%
  mutate(incidence_rate = str_replace(incidence_rate,"S","0")) %>%
  mutate(incidence_rate = as.numeric(incidence_rate)) ->
  incidence

incidence %<>% filter(sex=="Female")

cor_incidence_tmp <- data.frame(p_pearson=numeric(),p_adjusted=numeric(),cor=numeric(),
                                cancer=character(), feature=character(),comparisons=integer())


for (cancer in incidence$cancer %>% unique()){
  temp <- incidence_rf[[cancer]][[1]]$children
  t <- left_join(temp[,-3], incidence[incidence$cancer == cancer,], by = c('DHB','year'))
  comparisons<-0
  for(value in names(t)[4:15]){
    cor <- cor.test(t[[value]],t[['incidence_rate']])$estimate  
    p_pearson <- cor.test(t[[value]],t[['incidence_rate']])$p.value
    cor_incidence_tmp %<>% rbind(setNames(c(p_pearson,-1,cor,cancer,value,-1) %>% as.list(),names(cor_incidence_tmp)))
    comparisons<-comparisons+1
  }
  cor_incidence_tmp$p_adjusted[cor_incidence_tmp$cancer == cancer] <- p.adjust(
    cor_incidence_tmp$p_pearson[cor_incidence_tmp$cancer == cancer]) 
  cor_incidence_tmp[cor_incidence_tmp$cancer == cancer,'comparisons'] <- comparisons
}
cor_incidence_tmp$comparisons_across_feature <- nrow(cor_incidence_tmp)

cor_incidence_tmp$cor %<>% as.numeric()
cor_incidence_children <- cor_incidence_tmp
cor_incidence_tmp %<>% filter(p_adjusted <= 0.05, abs(cor) >= 0.2) %>% arrange(p_adjusted)

if(WRITE_FILES){
  setwd('D:\\data422')
  write.csv(cor_incidence_children,'data/correlation_result/cor_incidence_children.csv',row.names=F)
}



if(FALSE){
  library(ggplot2)
  ggplot(data = cor_incidence) +
    aes(x=cor , y=-log10(p_adjusted), color = cancer)+
    geom_point(size=1.5, alpha=0.6) + 
    geom_vline(xintercept = c(-0.2, 0.2), linetype = "dashed", color = "grey",size=0.8) +
    geom_hline(yintercept = 2, linetype = "dashed", color = "grey",size=0.8) 
  
  
  temp <- cor_incidence[grepl("^Child", cor_incidence$feature), ]
  
  ggplot(data = temp) +
    aes(x=cor , y=-log10(p_adjusted), color = cancer)+
    geom_point(size=1.5, alpha=0.6) + 
    geom_vline(xintercept = c(-0.2, 0.2), linetype = "dashed", color = "grey",size=0.8) +
    geom_hline(yintercept = 2, linetype = "dashed", color = "grey",size=0.8) 
}