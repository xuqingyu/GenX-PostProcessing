load_ts <- read_csv('./data/Total_load_by_region_DG_subtracted.csv')
for (i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  if (temp_total_title == 'PJM'){
    Scenario_list = c('Current Policy (CP)', 'Stated Policy (SP)', 'Deep Decarbonization (DD)')
  } else if (temp_total_title == 'New Jersey') {
    Scenario_list = c('Current Policy (CP)', 'Stated Policy (SP)')
  }
  temp_load <- load_ts %>%
    filter(GenX.Region %in% temp_total,Year %in% years) %>%
    group_by(SCENARIO, Year, LocalHourID) %>%
    summarize(`Com&Res Space Heating & Cooling Load` = sum(space_heat_MW),
           `Com&Res Water Heating Load` = sum(water_heat_MW),
           `Trans LDV Load` = sum(LDEV_MW),
           `Trans MDV/HDV/Bus Load` = sum(MHBEV_MW),
           `Other Load` = sum(Base_MW)) %>%
    rename(year = Year,SCENARIO_Load = SCENARIO) %>%
    pivot_longer(cols = c(`Com&Res Space Heating & Cooling Load`,
                          `Com&Res Water Heating Load`,
                          `Trans LDV Load`,
                          `Trans MDV/HDV/Bus Load`,
                          `Other Load`)) %>%
    rename(`Load Type` = name)
  
  temp_load <- left_join(load_mapping,temp_load) %>%
    select(-SCENARIO_Load) %>%
    left_join(cases_newnames) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity)) %>%
    filter(Scenario %in% Scenario_list,
           TechSensitivity %in% c('Mid')) %>%
    mutate(`Load Type` = factor(`Load Type`, levels = c('Trans LDV Load',
                                                        'Trans MDV/HDV/Bus Load',
                                                        'Com&Res Space Heating & Cooling Load',
                                                        'Com&Res Water Heating Load',
                                                        'Other Load')))
  
  ggplot()+
    geom_area(data=temp_load, aes(x=LocalHourID, y=value/1e3, fill=`Load Type`), position = position_stack()) +
    geom_hline(yintercept = 0, color = 'black') +
    facet_grid(year~Scenario) +
    scale_fill_brewer(palette = 'Set1')+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab('Load (GW)')+
    xlab('Hour')+
    guides(color = guide_legend(nrow = 1, title.position = "left"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Load_Timeseries_component_',temp_total_title,'_.png'),width = 12,height=7) 
  
}
