for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  # read in lse trade-off
  lse_cost_vs_emission <- read_csv(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/LSE_Cost_Emission_Tradeoff',temp_total_title,".csv")) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
  reference_row <- which((lse_cost_vs_emission$case=='currentpolicy_mid') & (lse_cost_vs_emission$year == 2019))
  reference_lse_cost <- lse_cost_vs_emission$`LSE Net Payment ($/MWh)`[reference_row]
  reference_lse_emission <- lse_cost_vs_emission$`Load Emissions Rate (Ton/MWh)`[reference_row]
  system_cost_vs_emission <- read_csv(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/System_Cost_Emission_Tradeoff',temp_total_title,".csv")) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
  for (j in 1:n_comparison){
    temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
    lse_cost_vs_emission_comparison <- lse_cost_vs_emission %>%
      filter(Scenario %in% temp_compared_scenario) %>%
      filter(year != 2019) %>%
      filter(TechSensitivity %in% interested_sensitivity)
    system_cost_vs_emission_comparison <- system_cost_vs_emission %>%
      filter(Scenario %in% temp_compared_scenario) %>%
      filter(TechSensitivity %in% interested_sensitivity)
    ggplot()+
      geom_point(data = lse_cost_vs_emission_comparison, aes(x = `Load Emissions Rate (Ton/MWh)`, y = `LSE Net Payment ($/MWh)`, 
                                                             color = TechSensitivity)) +
      scale_x_reverse()+
      scale_color_brewer(palette = 'Set1')+
      theme_bw()+
      theme(legend.position = "bottom")+
      guides(color = guide_legend(nrow = 2, title.position = "left"))+
      geom_vline(xintercept = reference_lse_emission)+
      geom_hline(yintercept = reference_lse_cost) +
      labs(caption = "Vertical and Horizontal lines show 2019 (simulated) emission/cost level")+
      facet_grid(Scenario~year) +
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSEvsEmission_',temp_total_title,"_",comparison[j],'.png'),width = 7.5,height=7) 
    ggplot()+
      geom_point(data = lse_cost_vs_emission_comparison, aes(x = `Load Emissions Rate (Ton/MWh)`, y = `LSE Net Payment (with DG, $/MWh)`, 
                                                             color = TechSensitivity)) +
      scale_x_reverse()+
      scale_color_brewer(palette = 'Set1')+
      theme_bw()+
      theme(legend.position = "bottom")+
      guides(color = guide_legend(nrow = 2, title.position = "left"))+
      geom_vline(xintercept = reference_lse_emission)+
      geom_hline(yintercept = reference_lse_cost) +
      labs(caption = "Vertical and Horizontal lines show 2019 (simulated) emission/cost level")+
      facet_grid(Scenario~year) +
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSEvsEmission_withDG_',temp_total_title,"_",comparison[j],'.png'),width = 7.5,height=7) 
    
    ggplot()+
      geom_point(data = system_cost_vs_emission_comparison, aes(x = `Load Emissions Rate (Ton/MWh)`, y = `System Cost ($/MWh)`, 
                                                                color = TechSensitivity)) +
      scale_x_reverse()+
      scale_color_brewer(palette = 'Set1')+
      theme_bw()+
      theme(legend.position = "bottom")+
      guides(color = guide_legend(nrow = 2, title.position = "left"))+
      geom_vline(xintercept = reference_lse_emission)+
      labs(caption = "Vertical line shows 2019 (simulated) emission level")+
      facet_grid(Scenario~year) +
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemvsEmission_',temp_total_title,"_",comparison[j],'.png'),width = 7.5,height=7)      
    ggplot()+
      geom_point(data = system_cost_vs_emission_comparison, aes(x = `Load Emissions Rate (Ton/MWh)`, y = `System Cost (with DG, $/MWh)`, 
                                                                color = TechSensitivity)) +
      scale_x_reverse()+
      scale_color_brewer(palette = 'Set1')+
      theme_bw()+
      theme(legend.position = "bottom")+
      guides(color = guide_legend(nrow = 2, title.position = "left"))+
      geom_vline(xintercept = reference_lse_emission)+
      labs(caption = "Vertical line shows 2019 (simulated) emission level")+
      facet_grid(Scenario~year) +
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemvsEmission_withDG_',temp_total_title,"_",comparison[j],'.png'),width = 7.5,height=7)      
    
  }
  
}