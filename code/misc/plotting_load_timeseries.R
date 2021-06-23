# Plotting load time series
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
    summarise(Total = sum(water_heat_MW + space_heat_MW + LDEV_MW + MHBEV_MW + Base_MW)) %>%
    rename(year = Year,SCENARIO_Load = SCENARIO)

  temp_load <- left_join(load_mapping,temp_load) %>%
    select(-SCENARIO_Load) %>%
    left_join(cases_newnames) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity)) %>%
    filter(Scenario %in% Scenario_list,
           TechSensitivity %in% c('Mid'))
  ggplot()+
    geom_line(data=temp_load, aes(x=LocalHourID,y=Total/1e3,color=Scenario)) +
    scale_color_brewer(palette = 'Set1')+
    theme_bw()+
    theme(legend.position = "bottom")+
    ylab('Total Load (GW)')+
    ggtitle(paste0('Total Load of ', temp_total_title))+
    guides(color = guide_legend(nrow = 1, title.position = "left"))+
    facet_grid(year~Scenario) +
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Load_Timeseries',temp_total_title,'_.png'),width = 7.5,height=7) 
  
}

# some extra calculation of the peak and top 5% load
temp_load_peak <- temp_load %>%
  group_by(Scenario,TechSensitivity,year) %>%
  summarize(Peak = max(Total),
            `Rank 5%` = quantile(Total, 0.95))

rm(temp_load, load_ts)
