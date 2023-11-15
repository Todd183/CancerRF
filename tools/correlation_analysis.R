## correlation analysis
load('data/clean/rf.Rdata')
incidence = read_csv("data/clean/incidence_sexfiltered.csv")


categories = unique(rf$category)
# cancers = unique(incidence$cancer)
cancers = c('Prostate','Colorectal','Lung','Melanoma','Breast')

pairs = crossing(categories,cancers) %>% pmap(.,list) # create category and cancer pairs and save it in a list

lapply(pairs,function(pair){
  
  df_cancer = incidence %>% filter(cancer == pair$cancers)
  df_rfs = rf %>% filter(category == pair$categories)
  
  
  if(pair$categories == 'Birth Number' & pair$cancer != "Breast"){ #Birth number only map to female cancer "Breast"
    return(NULL)
  }else if( pair$categories == 'NZHS'){
    df = inner_join(df_cancer,df_rfs,by=c("DHB","year","sex"))
  }else {
    df_cancer = df_cancer %>% select(-sex)
    df_rfs = df_rfs %>% select(-sex)
    df = inner_join(df_cancer ,df_rfs,by=c("DHB","year"))
  }
  
  df %>% 
    group_by(rf) %>%
    summarise(cor = cor.test(value,incidence_rate,method = 'pearson')[4][[1]][[1]],
              pvalue = cor.test(value,incidence_rate,method = 'pearson')[3][[1]][[1]]) %>%
    ungroup(.) %>%
    mutate(cancer = pair$cancers,category = pair$categories,.before = 1) %>%
    mutate(p.adj = p.adjust(pvalue, method = 'BH')) -> res
  return(res)
}) %>% 
  do.call(rbind,.) %>%
  mutate(sign = ifelse(cor > 0,"pos","neg"),
         group = case_when(category %in% c("Air quality","Earthquake","Water","Temperature") ~ "Environment",
                           TRUE ~ "Social"),
         group2 = case_when(category == "NZHS" ~ rf,
                            TRUE ~ category)
  ) -> cor.res

##visulization



##
library(tools)
library(tidyverse)

CAs = list(c("Prostate","Breast","Colorectal"),"Lung","Melanoma") #only view results of top 5 cancers
#### 1. volcano plot ####


# filter significant result for ggplot point
sig.point <- cor.res %>%
  filter(p.adj < 0.05, abs(cor) >= 0.3) #significant results are defined as abs(r) >= 0.3 and p.adj < 0.05

# filter non-significant result for ggplot point
non.sig.point <- cor.res %>%
  filter(p.adj >= 0.05, abs(cor) < 0.3) #significant results are defined as asb(r) < 0.3 and p.adj >= 0.05#

cols = ggsci::pal_simpsons("springfield")(16)
cols = cols[-3] # remove grey color


#filter most relevant RFs for labels in ggplot
labels <- sig.point %>%
  group_by(category) %>%
  top_n(1,abs(cor))

#draw volcano plot
cor.res %>% 
  ggplot(aes(x=cor , y=-log10(p.adj))) +
  geom_vline(xintercept = c(-0.3, 0.3), linetype = "dashed", color = "grey",size=0.8) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey",size=0.8)+
  geom_point(sig.point, mapping = aes(x=cor , y=-log10(p.adj),color = cancer,shape = group), size=1.5, alpha=0.8) + 
  scale_color_manual(values = cols)+
  geom_point(non.sig.point,mapping = aes(x=cor , y=-log10(p.adj)),color = 'grey', size=1.5, alpha=0.2) + 
  ggrepel::geom_label_repel(labels,mapping = aes(x=cor , y=-log10(p.adj),color = cancer,label = group2),
                            show.legend = F,
                            min.segment.length = 0,
                            force_pull = 1.5)+
  labs(y = "-log10(P.adj)", x = "Correlation R")+
  theme_classic() -> volcanoPlot

#### 2. Tables for RFs in five most common cancer types ####
#result visualized with tables for corresponding cancer types
#Less risk factors for c("Prostate","Breast","Colorectal"), shown in a single table for these three cancers

sig.tables <- list()
ftab <- list()


for(i in 1:length(CAs)){
  ca = CAs[[i]]
  
  #set filters of variables
  #some variables are similar, such as 'Healthy weight','Mean weight (kg)', 'Overweight or obese', these are all related to the same RF (overweight), and thus filtered for better demontration
  rf_filters=c()
  if( "Lung" %in% ca){
    rf_filters = c("Obese class 1","Obese class 2","Obese class 3",
                    "Obese",
                    'Overweight or obese',
                    'Teeth removed due to decay in past 12 months',
                    'Teeth removed due to decay in lifetime',
                    'Waist to height ratio ≥ 0.5',
                    'Mean weight (kg)',
                    'Mean waist (cm)',
                    'All teeth removed due to decay',
                    'Healthy weight',
                    'Daily smokers'
    )
  }else if("Breast" %in% ca){
    rf_filters = c('Heavy episodic drinking at least monthly (past-year drinkers)',
                    "Heavy episodic drinking at least monthly (total population)",
                    "Heavy episodic drinking at least weekly (past-year drinkers)")
  }
  
  
  sig.point %>%
    filter(!rf %in% rf_filters) %>% #filter similar variables
    filter(!(category == "Temperature" & rf != "Average_Annual")) %>% # for temperature variables, only show the annual average
    filter(cancer %in% ca ) %>% # filter by cancer types
    group_by(category,group2,cancer) %>% 
    top_n(1,abs(cor))%>% #filter the most relevant RFs based on group2 for each cancer type
    ungroup(.)%>%
    mutate(
      Cancer = cancer,
      Group = group,
      Category = category,
      temp_rf = rf,
      Correlation = case_when(sign == "pos" ~ '+',sign == "neg" ~ "-"),
      temp_rf = toTitleCase(str_replace_all(temp_rf,'_'," ")), #capitalize first character of each word in variabel
      temp_rf = str_replace_all(temp_rf,'(?<=\\().+?(?=\\))',tolower), #words within () remains lower format
    ) %>%
    select(Cancer,Correlation,Group,Category,temp_rf,cor,p.adj,rf) %>%
    arrange(factor(Cancer,levels = c("Lung","Melanoma","Breast","Prostate","Colorectal")),Correlation) %>%
    rename(r = cor,`Risk Factors` = temp_rf) -> sig.tables[[i]]
  
  sig.tables[[i]] %>%
    select(-rf) %>%
    rempsyc::nice_table(short = F) %>%
    flextable::line_spacing(space = 0.8) -> ftab[[i]] #Here table index starts from 2 and thus use i+1
  # print(ftab[[i]])
}




sig.table <- do.call(rbind,sig.tables)

corplot <- function(ca,incidence){
  
  rfs <- sig.table %>% 
    filter(Cancer == ca) 
  
  df_cancer = incidence %>% filter(cancer == ca)
  df_rf = rf %>% filter(rf %in% rfs$rf)
  
  inner_join(df_cancer,df_rf) %>%
    left_join(rfs[,c("Risk Factors","rf")],by = "rf") %>%
    mutate(gg_group = paste(category,'-',rf),
           gg_group = str_replace(gg_group,"NZHS - ",'')) %>%
    ggplot(.,aes(x = value, y =incidence_rate)) +
    geom_point(aes(color = gg_group),alpha = 0.3) +
    scale_color_manual(values = cols)+
    geom_smooth(aes(color = gg_group),method='lm')+
    ggpubr::stat_cor( 
      label.x.npc = "left",
      label.y.npc = 'top') +
    facet_wrap(~gg_group,scales = 'free_x')+
    theme_classic()+
    ggtitle(paste(ca,"Cancer"))+
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 0.5))
}

corplot.CRC = corplot("Colorectal",incidence)
corplot.Lung = corplot("Lung",incidence)



