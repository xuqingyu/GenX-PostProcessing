# Load component ----
for (i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  temp_load_combined <- read_csv(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Load/Load_Component_",Subregions[i],".csv")) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
  if (temp_total_title == 'PJM'){
    Scenario_list = c('Current Policy (CP)', 'Stated Policy (SP)', 'Deep Decarbonization (DD)')
    bar2019 = as_tibble_row(c(name = '2019 Reference = 800 TWh', TWh = 800))
  } else if (temp_total_title == 'New Jersey') {
    Scenario_list = c('Current Policy (CP)', 'Stated Policy (SP)')
    bar2019 = as_tibble_row(c(name = '2019 Reference = 83 TWh', TWh = 83))
  }
  
  temp_load_bar <- temp_load_combined %>%
    filter(Scenario %in% Scenario_list,
           !(`Load Type` %in% c('Gross Total', 'Distributed Solar', 'Total', 'Total (after Temporal Resolution Reduction)')),
           TechSensitivity %in% c('Mid')) %>%
    mutate(`Load Type` = factor(`Load Type`, levels = c('Trans LDV Load',
                                                        'Trans MDV/HDV/Bus Load',
                                                        'Com&Res Space Heating & Cooling Load',
                                                        'Com&Res Water Heating Load',
                                                        'Other Load')))
  temp_load_bar_total <- temp_load_combined %>%
    filter(Scenario %in% Scenario_list,
           `Load Type` == 'Gross Total',
           TechSensitivity %in% c('Mid'))
  
  ggplot()+
    geom_col(data=temp_load_bar, aes(x=year,y=TWh,fill=`Load Type`)) +
    geom_point(data=temp_load_bar_total, aes(x=year,y = TWh)) +
    geom_text(data=temp_load_bar_total, aes(x=year,y = TWh,label = round(TWh,0)),nudge_y = 3)+
    geom_hline(data = bar2019, aes(yintercept = as.numeric(TWh), linetype = name), show.legend = F)+
    scale_fill_brewer(palette = 'Set1')+
    theme_bw()+
    theme(legend.position = "bottom")+
    guides(fill = guide_legend(nrow = 2, title.position = "left"))+
    labs(caption = paste0("LDV/MDV/HDV = Light/Medium/Heavy-Duty Vehicle\n horizontal line shows the 2019 level = ", bar2019$TWh, " TWh"))+
    geom_hline(yintercept = 0)+
    facet_grid(.~Scenario) +
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Load_Component_TWh',temp_total_title,'_.png'),width = 7.5,height=7) 
    
}
