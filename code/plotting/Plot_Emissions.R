
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  emissions_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Emissions/Emissions_',temp_total_title,"_with2019.csv")
  if (file.exists(emissions_subregion_fn)){
    emissions_subregion <- read_csv(emissions_subregion_fn)
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      emissions_subregion_comparison <- emissions_subregion %>%
        filter(Scenario %in% temp_compared_scenario)
      emissions_subregion_comparison$Scenario <- factor(emissions_subregion_comparison$Scenario, levels = scenario)
      emissions_subregion_comparison$TechSensitivity <- factor(emissions_subregion_comparison$TechSensitivity, levels = tech_sensitivity)
      
      ggplot()+
        geom_point(data = emissions_subregion_comparison,
                   aes(x = year, y = `Emissions (Mtons)`, color = `TechSensitivity`)) +
        geom_line(data = emissions_subregion_comparison,
                  aes(x = year, y = `Emissions (Mtons)`, color = `TechSensitivity`)) +
        facet_grid(.~Scenario) + 
        labs(title = comparison[j]) +
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Emission_',comparison[j],'_',temp_total_title,'Absolute_Emission.png'),width = 10,height=10)
      
      ggplot()+
        geom_point(data = emissions_subregion_comparison,
                   aes(x = year, y = `Generation Emissions Rate (Ton/MWh)`, color = `TechSensitivity`)) +
        geom_line(data = emissions_subregion_comparison,
                  aes(x = year, y = `Generation Emissions Rate (Ton/MWh)`, color = `TechSensitivity`)) +
        facet_grid(.~Scenario) + 
        labs(title = comparison[j]) +
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Emission_',comparison[j],'_',temp_total_title,'Gen_ER.png'),width = 10,height=10)
      
      ggplot()+
        geom_point(data = emissions_subregion_comparison,
                   aes(x = year, y = `Load Emissions Rate (Ton/MWh)`, color = `TechSensitivity`)) +
        geom_line(data = emissions_subregion_comparison,
                  aes(x = year, y = `Load Emissions Rate (Ton/MWh)`, color = `TechSensitivity`)) +
        facet_grid(.~Scenario) + 
        labs(title = comparison[j]) +
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Emission_',comparison[j],'_',temp_total_title,'Load_ER.png'),width = 10,height=10)
      
    }
  }
}