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

save(p_values_incidence,file = 'data/correlation_result/p_values_incidence.Rdata')
save(p_values_mortality, file = 'data/correlation_result/p_values_mortality.Rdata')

write.csv(p_values_incidence,'data/correlation_result/p_values_incidence.csv')
write.csv(p_values_mortality,'data/correlation_result/p_values_mortality.csv')

           