
##
library(tools)
library(tidyverse)

CAs = list(c("Prostate","Breast","Colorectal"),"Lung","Melanoma") #only view results of top 5 cancers
#### 1. volcano plot ####


cor_mortality <- read_csv('data/correlation_result/cor_mortality.csv')
cor_incidence <- read_csv('data/correlation_result/cor_incidence.csv')

cor_incidence <- cor_incidence %>% 
  filter(cancer %in% c("Prostate","Breast","Colorectal","Lung","Melanoma")) %>%
  filter(!is.na(p_value)) %>%
  separate(feature,into=c("group",'var'),sep ='-',extra = 'merge',remove = FALSE) %>% #separate original variable, extract the group info and variable info
  mutate(sign = ifelse(cor > 0,"pos","neg"),
         category = case_when(group %in% c("Air","Earthquake","Water","Temperature") ~ "Environment",
                              TRUE ~ "Humanities"),
         group2 = case_when(group == "NZHS" ~ var,
                            TRUE ~ group)
  ) %>%
  rename(Cancer = cancer, Category = category) 

# filter significant result for ggplot point
sig.point <- cor_incidence %>%
  filter(p_adjusted < 0.05, abs(cor) >= 0.3) #significant results are defined as abs(r) >= 0.3 and p.adj < 0.05

# filter non-significant result for ggplot point
non.sig.point <- cor_incidence %>%
  filter(p_adjusted >= 0.05, abs(cor) < 0.3) #significant results are defined as asb(r) < 0.3 and p.adj >= 0.05#

cols = ggsci::pal_simpsons("springfield")(16)
cols = cols[-3] # remove grey color


#filter most relevant RFs for labels in ggplot
labels <- sig.point %>%
  group_by(group) %>%
  top_n(1,abs(cor))

#draw volcano plot
cor_incidence %>% 
  ggplot(aes(x=cor , y=-log10(p_adjusted))) +
  geom_vline(xintercept = c(-0.3, 0.3), linetype = "dashed", color = "grey",size=0.8) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey",size=0.8)+
  geom_point(sig.point, mapping = aes(x=cor , y=-log10(p_adjusted),color = Cancer,shape = Category), size=1.5, alpha=0.8) + 
  scale_color_manual(values = cols)+
  geom_point(non.sig.point,mapping = aes(x=cor , y=-log10(p_adjusted)),color = 'grey', size=1.5, alpha=0.2) + 
  ggrepel::geom_label_repel(labels,mapping = aes(x=cor , y=-log10(p_adjusted),color = Cancer,label = group2),
                            show.legend = F,
                            min.segment.length = 0,
                            force_pull = 1.5)+
  labs(y = "-log10(P.adj)", x = "Correlation R")+
  theme_classic() 

#### 2. Tables for RFs in five most common cancer types ####
#result visualized with tables for corresponding cancer types
#Less risk factors for c("Prostate","Breast","Colorectal"), shown in a single table for these three cancers

sig.tables <- list()



for(i in 1:length(CAs)){
  ca = CAs[[i]]
  
  #set filters of variables
  #some variables are similar, such as 'Healthy weight','Mean weight (kg)', 'Overweight or obese', these are all related to the same RF (overweight), and thus filtered for better demontration
  var_filters=c()
  if( "Lung" %in% ca){
      var_filters = c("Obese class 1","Obese class 2","Obese class 3",
                      'Overweight or obese',
                      'Teeth removed due to decay in past 12 months',
                      'Waist to height ratio ≥ 0.5',
                      'Mean weight (kg)',
                      'Mean waist (cm)',
                      'All teeth removed due to decay',
                      'Healthy weight',
                      'Daily smokers'
                      )
  }else if("Breast" %in% ca){
    var_filters = c('Heavy episodic drinking at least monthly (past-year drinkers)',
                    "Heavy episodic drinking at least monthly (total population)",
                    "Heavy episodic drinking at least weekly (past-year drinkers)")
  }
  
  
  sig.point %>%
    filter(!var %in% var_filters) %>% #filter similar variables
    # filter(abs(cor) > 0.3) %>% # show correlation RF with r > 0.3
    filter(!(group == "Temperature" & var != "Average_Annual")) %>% # for temperature variables, only show the annual average
    filter(Cancer %in% ca ) %>% # filter by cancer types
    group_by(Category,group2,Cancer) %>% 
    top_n(1,abs(cor))%>% #filter the most relevant RFs based on group2 for each cancer type
    ungroup(.)%>%
    mutate(
      # sign = ifelse(cor > 0,"pos","neg"), #correlation direction, positive or negative
      # category = case_when(group %in% c("Air","Earthquake","Water","Temperature") ~ "Environment", # set two category, Environment and Humanities
      #                      TRUE ~ "Humanities"),
      # group2 = case_when(group == "NZHS" ~ var, #NZHS has many different group, here show NZHS based on the variable as group
      #                    TRUE ~ group),
      Correlation = case_when(sign == "pos" ~ '+',sign == "neg" ~ "-"),
      var = toTitleCase(str_replace_all(var,'_'," ")), #capitalize first character of each word in variabel
      var = str_replace_all(var,'(?<=\\().+?(?=\\))',tolower), #words within () remains lower format
    ) %>%
    select(Cancer,Correlation,Category,group,var,cor,p_adjusted,feature) %>%
    #modify###########
    arrange(factor(Cancer,levels = c("Lung","Melanoma","Breast","Prostate","Colorectal")),Correlation) %>%
    rename(r = cor, p.adj = p_adjusted, `Risk Factors` = var, Group = group) -> sig.tables[[i]]
  
  sig.tables[[i]] %>%
    select(-feature) %>%
    rempsyc::nice_table(short = F,title = paste0( "Table",i+1,". Risk factors of ", paste(ca,collapse = ', '))) %>%
    flextable::line_spacing(space = 0.8)-> tab #Here table index starts from 2 and thus use i+1
    # flextable::theme_vanilla() %>%
    # flextable::merge_v(j = c("Cancer", "Category","Group"))-> tab
  print(tab)

}


## colorectal

ca = "Colorectal"
sig.table <- do.call(rbind,sig.tables)

corplot <- function(ca){
  load('data/clean/incidence_rf.Rdata')
  rfs <- sig.table %>% 
    filter(Cancer == ca) %>%
    mutate(group = case_when(Group == "Temperature" ~"tmp",
                             TRUE ~Group),
           group = tolower(group))
  
  lapply(1:nrow(rfs),function(i){
    group = rfs$group[i]
    feature = rfs$feature[i]
    df = incidence_rf[[ca]][[1]][[group]][,c("year",'DHB','incidence_rate',feature)] 
    colnames(df) = c("Year","DHB","rate",'value')
    df %>% mutate(RFs = rfs$`Risk Factors`[i], 
                  Category = rfs$Category[i], 
                  Group = rfs$Group[i], 
                  gg_group = paste(Group,'-',RFs),
                  gg_group = str_replace(gg_group,"NZHS - ",'')) %>%
      filter(!is.na(value))
  }) %>% do.call(rbind,.) %>%
    ggplot(.,aes(x = value, y =rate)) +
    geom_point(aes(color = gg_group),alpha = 0.3) +
    scale_color_manual(values = cols)+
    geom_smooth(aes(color = gg_group))+
    ggpubr::stat_cor( 
      label.x.npc = "left",
      label.y.npc = 'top') +
    facet_wrap(~gg_group,scales = 'free_x')+
    theme_classic()+
    ggtitle(paste(ca,"Cancer"))+
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 0.5))

}

corplot("Colorectal")
corplot("Lung")






