library(tidyverse)
library(magrittr)


setwd('D:\\data422')
load(file = 'data/clean/incidence_rf.Rdata')
load(file = 'data/clean/mortality_rf.Rdata')


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

save(p_values_incidence,file = 'data/correlation_result/p_values_incidence.Rdata')
save(p_values_mortality, file = 'data/correlation_result/p_values_mortality.Rdata')





 
cor_incidence <- data.frame(p_pearson=numeric(),cor=numeric(),cancer=character(),feature=character())
for(cancer in names(incidence_rf)){
  for(feature in names(incidence_rf[[cancer]][[1]])){
    temp <- incidence_rf[[cancer]][[1]][[feature]]
    for(value in names(temp)[4:length(temp)]){
      cor <- cor.test(temp[[value]],temp[['incidence_rate']])$estimate  
      p_pearson <- cor.test(temp[[value]],temp[['incidence_rate']])$p.value
      cor_incidence %<>% rbind(setNames(c(p_pearson,cor,cancer,value) %>% as.list(),names(cor_incidence)))
    }
  }
}
cor_incidence %<>% left_join(p_values_incidence, by = c('cancer','feature'))



cor_mortality <- data.frame(p_pearson=numeric(),cor=numeric(),cancer=character(),feature=character())
for(cancer in names(mortality_rf)){
  for(feature in names(mortality_rf[[cancer]][[1]])){
    temp <- mortality_rf[[cancer]][[1]][[feature]]
    for(value in names(temp)[4:length(temp)]){
      cor <- cor.test(temp[[value]],temp[['mortality_rate']])$estimate  
      p_pearson <- cor.test(temp[[value]],temp[['mortality_rate']])$p.value
      cor_mortality %<>% rbind(setNames(c(p_pearson,cor,cancer,value) %>% as.list(),names(cor_mortality)))
    }
  }
}
cor_mortality %<>% left_join(p_values_incidence, by = c('cancer','feature'))

cor_incidence$cor %<>% as.numeric()
cor_mortality$cor %<>% as.numeric()

write.csv(cor_mortality,'data/correlation_result/cor_mortality.csv')
write.csv(cor_incidence,'data/correlation_result/cor_incidence.csv')

##################################
#  number of children   female   #
##################################
library(ggplot2)
ggplot(data = cor_incidence) +
  aes(x=cor , y=-log10(p_adjusted))+
  geom_point()


class(cor_incidence$cor)

