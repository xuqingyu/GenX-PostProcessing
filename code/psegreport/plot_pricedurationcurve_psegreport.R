# Duration curve
energyprice_ts <- read_csv(paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries.csv')) %>%
  left_join(cases_newnames,by=c('case' = 'case_description')) %>%
  mutate(Scenario = factor(Scenario, levels = scenario),
         TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
         year = factor(year, levels = years))

for (j in 1:n_comparison){
  for (k in 1:length(interested_sensitivity)){
    temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
    temp_plot <- energyprice_ts  %>%
      filter(Scenario %in% temp_compared_scenario,
             Region %in% c('PJM_NJLand','PJM_NJCoast'),
             TechSensitivity == interested_sensitivity[k]) %>%
      arrange(Scenario, Region, Zone, case, year, desc(Price))
    rank = rep(c(1:8736),length.out = nrow(temp_plot))
    temp_plot = cbind(temp_plot,rank)
    
    ggplot(data=temp_plot,aes(x=rank, y=Price, color = Scenario)) +
      scale_color_brewer(palette = 'Set1') +
      geom_line()+
      # theme_bw()+
      facet_wrap(Region~year) + 
      geom_hline(yintercept = 0, color = 'grey30')+
      theme(legend.position = 'bottom') +
      coord_cartesian(ylim = c(-50,200))+
      ggtitle(paste0('Energy Price Duration curves Under \nSensitivity ',interested_sensitivity[k]))+
      ylab("Energy Price ($/MWh)") + 
      ggsave(paste0(RunFdr,'/Graphics/EnergyPrice/Duration Curve Energy Price of Region ',comparison[j],'_',k,'.eps'),
             width = 10,
             height= 7,
             device = 'eps')
  }
}
