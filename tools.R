
library(ComplexHeatmap)
library(circlize)
col_jama = ggsci::pal_jama(palette = "default")(7)
cols_heatmap = pal_nejm("default")(7)

DHB_TO_ISLANDS <- c(
  "Auckland" = "North",
  "Bay of Plenty" = "North",
  "Canterbury" = "South",
  "Capital & Coast" = "North",
  "Counties Manukau" = "North",
  "Hawke's Bay" = "North",
  "Hutt Valley" = "North",
  "Lakes" = "North",
  "MidCentral" = "North",
  "Nelson Marlborough" = "South",
  "Northland" = "North",
  "South Canterbury" = "South",
  "Southern" = "South",
  "Tairawhiti" = "North",
  "Taranaki" = "North",
  "Waikato" = "North",
  "Wairarapa" = "North",
  "Waitemata" = "North",
  "West Coast" = "South",
  "Whanganui" = "North"
)


#ggplot from grob
ggplot_from_grob <- function(grob) {
  grid.newpage()
  grid.draw(grob)
  grid.force()
  plot <- ggplot()
  invisible(plot)
}

# set color strip
draw_color_strip <- function(gg,colors){
  g <- ggplot_gtable(ggplot_build(gg))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- colors
  k <- 1
  for (i in strip_both) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k+1
  }
  grid::grid.draw(g)
  return(g)
}


# barplot based on sex group
draw_sex_variance <- function(data,type,fulltitle=FALSE){
  if(fulltitle == T){
    title = paste0("Average ", type," Rate across different cancer types during 2011-2020 in NZ")
  }else{
    title=type
  }
  rate_name = colnames(data)[grepl('rate',colnames(data))]
  data %>%
    rename(rate = rate_name) %>%
    filter(DHB == "All New Zealand") %>%
    group_by(year,cancer,sex) %>%
    summarise(rate_mean = mean(rate,na.rm=T)) %>%
    pivot_wider(names_from = sex, values_from = rate_mean ) %>%
    mutate(group = case_when(is.na(Male) & !is.na(Female) ~ "Female only",
                             !is.na(Male) & is.na(Female) ~ "Male only",
                             TRUE ~ "Both gender")) %>%
    ggplot(aes(x = cancer)) +
    geom_bar(aes(y = Male, fill = "Male"), position = "dodge", stat = "identity") +
    geom_bar(aes(y = -Female, fill = "Female"), position = "dodge", stat = "identity") +
    coord_flip() +  # Flip the coordinates to have horizontal bars
    labs(fill = "Gender",y= paste0("Average ",type," Rate (per 100,000 people)"),x="") +  # Add a legend label for fill
    scale_fill_manual(values = c("Male" = col_jama[1], "Female" = col_jama[2])) +
    scale_y_continuous(labels = abs) + 
    theme_classic() +
    facet_grid(group~.,scales = 'free') +  
    ggtitle(title)+
    theme(
      legend.position = 'top',
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(face='bold'),
      axis.title.x  = element_text(face='bold'),
      strip.text.y = element_text(color = "white")
    ) -> gg
  
  colors =  c("darkgrey",col_jama[2],col_jama[1])
  g = draw_color_strip(gg, colors)
  return(g)
}


#draw regional cancer heatmap
regional_heatmap <- function(data, type){
  data = data %>% mutate(cancer = str_replace(cancer,'Non-Hodgkin lymphoma','NHL'))  
  rate_name = colnames(data)[grepl('rate',colnames(data))]
  data %>%
    arrange(sex) %>%
    rename(rate = rate_name) %>%
    filter(DHB != "All New Zealand") %>%
    group_by(DHB,cancer) %>%
    summarise(rate_mean = mean(rate,na.rm=T)) %>%
    pivot_wider(names_from = cancer, values_from = rate_mean ) %>% as.data.frame(.) -> hdata
  
  #color panel for plotting
  cols_heatmap = pal_nejm("default")(7)
  cols_anno = pal_jama("default")(7)
  
  rownames(hdata) = hdata$DHB
  
  
  colnames(hdata)
  cancer_to_sex_df <- data %>%
    select(cancer,sex) %>%
    filter(!duplicated(cancer))
  cancer_to_sex <- cancer_to_sex_df$sex
  names(cancer_to_sex) <-cancer_to_sex_df$cancer
  cancer_to_sex <- sort(cancer_to_sex)
  hdata = hdata[,names(cancer_to_sex)]
  
  mat = as.matrix(hdata[,-1])
  
  
  column_ha = HeatmapAnnotation(
    gender = cancer_to_sex[colnames(mat)],
    col = list(
      gender = c("Male" = col_jama[1], "Female" = col_jama[2], "AllSex" = "darkgrey")
    ),
    gp = gpar(col = "black",lex=0.5),
    show_annotation_name = T)
  
  
  col_fun =  colorRamp2(c(min(rowSums(mat)), mean(rowSums(mat)) ,max(rowSums(mat))), c(cols_heatmap[2],cols_heatmap[7],cols_heatmap[1]))
  
  rsp = DHB_TO_ISLANDS
  
  row_ha = rowAnnotation(
    Total = rowSums(mat),
    # "Total cancer rate" = anno_barplot(rowSums(mat),gp = gpar(fill = col_fun)),
    col = list(Total = col_fun),
    gp = gpar(col = "black",lex=0.5),
    show_annotation_name = F,
    show_legend= F)
  
  
  ht = Heatmap(mat,
               # column_split = csp,
               column_title_side = 'bottom',
               cluster_rows = T,
               cluster_columns = T,
               show_column_dend = FALSE, 
               show_row_dend = FALSE,
               column_names_side = 'top',
               column_names_rot = 90,
               row_split = rsp,
               # col = colorRamp2(c(min(mat),mean(mat),max(mat)),  c("#004729","#9DBF9E", "#FFFFE3")),
               col = colorRamp2(c(min(mat), mean(mat) ,max(mat)), c(cols_heatmap[2],cols_heatmap[7],cols_heatmap[1])),
               # col = colorRamp2(c(min(mat), mean(mat) ,max(mat)), c("#9DBF9E", "#FCB97D", "#A84268")),
               rect_gp = gpar(col = "black", lwd = 0.3),
               width = ncol(mat)*unit(7, "mm"), 
               height = nrow(mat)*unit(5, "mm"),
               heatmap_legend_param = list(
                 title = paste(type,"Rate (per 100,000 people)"),
                 title_position = 'leftcenter-rot',
                 legend_direction = c("vertical"),
                 legend_height = unit(6, "cm"),
                 legend_postion = "top"
               ),
               top_annotation = column_ha,
               right_annotation = row_ha
  )
  return(ht)
}



cancer_region_map <- function(data,type,map){
  rate_name = colnames(data)[grepl('rate',colnames(data))]
  
  data %>%
    rename(rate = rate_name) %>%
    mutate(cancer = str_replace(cancer,'Non-Hodgkin lymphoma','NHL')) %>%
    filter(DHB != "All New Zealand") %>%
    group_by(DHB,cancer) %>%
    summarise(rate = mean(rate)) %>%
    group_by(cancer) %>%
    top_n(1,rate) %>%
    group_by(DHB) %>%
    summarise(cancers = paste0(cancer, " (", round(rate,digits = 1),") ",collapse = "\n· ")) -> top_cancer_DHB
  
  data %>%
    rename(rate = rate_name) %>%
    mutate(cancer = str_replace(cancer,'Non-Hodgkin lymphoma','NHL')) %>%
    filter(DHB != "All New Zealand") %>%
    group_by(DHB,cancer) %>%
    summarise(rate = mean(rate)) %>%
    group_by(DHB) %>%
    summarise(total_rate = sum(rate)) -> overall_rate
  
  DHB_map2 <- DHB_map %>% 
    mutate(DHB = case_when(DHB_name == "Capital and Coast" ~ "Capital & Coast", #modify DHB names in DHB_mapto match cancer
                           TRUE ~ DHB_name)) %>%
    select( DHB,geometry) %>%
    left_join(., top_cancer_DHB) %>%
    left_join(., overall_rate) %>%
    mutate(label = paste0(DHB," (",total_rate,") ",'\n· ',cancers))
  
  label <- DHB_map2 %>% filter(!is.na(cancers))
  
  if(type == "Incidence"){
    label2 <- label %>% filter(DHB %in% c("Auckland","Counties Manukau",'Taranaki','Whanganui'))
    label1 <- label %>% filter(DHB %in% c("Southern","Tairawhiti","Hawke's Bay","MidCentral",'Hutt Valley','Capital & Coast'))
  }else{
    label2 <- label[1:nrow(label)/2-1,]
    label1 <- label[nrow(label)/2 :  nrow(label),]
  }
  axis_limits=  c(165,-50,179,-30)
  x_scale = (axis_limits[3] - axis_limits[1])
  
  ggmap <- ggplot(DHB_map2) +
    geom_sf(aes(fill = total_rate)) +
    coord_sf(xlim = c(165,188), ylim = c(-50,-30))+
    scale_fill_gradientn(
      # colors = c("#004729","#9DBF9E", "#FFFFE3"),
      colors =  c(cols_heatmap[2],cols_heatmap[7],cols_heatmap[1]),
      #colors = c(cols_heatmap[2],cols_heatmap[7],cols_heatmap[1]),
      # colors = c("#9DBF9E", "#FCB97D", "#A84268"),
      na.value = "grey80",
      limits = c(min(DHB_map2$total_rate), max(DHB_map2$total_rate)),
      oob = scales::squish,
      name = paste("  ",type,"Rate\n(per 100,000 people)")
    ) +
    geom_label_repel(
      data = label1,
      aes(geometry = geometry,label = label, fill = total_rate),
      size = 2.5,
      segment.size = 0.2,
      stat = "sf_coordinates",
      hjust = 0,
      direction='y',
      xlim = c(axis_limits[3] + x_scale * 0.2, axis_limits[3]+x_scale * 0.5)
    ) +
    geom_label_repel(
      data = label2,
      aes(geometry = geometry,label = label, fill = total_rate),
      size = 2.5,
      segment.size = 0.2,
      stat = "sf_coordinates",
      hjust = 0,
      direction='y',
      xlim = c(axis_limits[1] - x_scale * 0.2, axis_limits[3] - x_scale * 0.5),
    ) +
    # geom_label_repel(
    #   data = label2,
    #   aes(geometry = geometry,label = label, fill = total_rate),
    #   size = 2.5,
    #   segment.size = 0.2,
    #   stat = "sf_coordinates",
    #   hjust = 0,
    #   direction='y',
    #   xlim = c(axis_limits[1] - x_scale * 0.2, axis_limits[3] - x_scale * 0.5),
    # ) +
    geom_point(
      data = label,
      aes(geometry = geometry),
      size = 2,
      color = 'grey',
      stat = "sf_coordinates"
    ) +
    geom_point(
      data = label,
      aes(geometry = geometry),
      size = 1,
      color =  'white',
      stat = "sf_coordinates"
    ) +
    # scale_color_viridis_c(option = "C") +
    # theme(legend.position = "bottom")+
    coord_sf(xlim = c(axis_limits[1] - x_scale * 0.2,axis_limits[3] + x_scale * 0.5), ylim = c(axis_limits[2],axis_limits[4])) +
    theme_void(base_family = "serif") +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.justification = c(0, 0.2),
      legend.position = c(0.38, 0.1),
      legend.title = element_text(vjust = 1),
      legend.direction = 'horizontal'
    )
  return(ggmap)
}




