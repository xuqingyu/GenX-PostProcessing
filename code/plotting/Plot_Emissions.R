source('./code/Header.R')
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  emissions_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Emissions/Emissions_',temp_total_title,"_with2019.csv")
  if (file.exists(emissions_subregion_fn)){
    emissions_subregion <- read_csv(emissions_subregion_fn) %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
    reference_row <- which((emissions_subregion$case=='currentpolicy_mid') & (emissions_subregion$year == 2019))
    reference_emissions <- emissions_subregion$`Emissions (Mtons)`[reference_row]
    reference_LER <- emissions_subregion$`Load Emissions Rate (Ton/MWh)`[reference_row]
    reference_GER <- emissions_subregion$`Generation Emissions Rate (Ton/MWh)`[reference_row]
    emissions_subregion <- emissions_subregion %>%
      filter(year != 2019)
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      emissions_subregion_comparison <- emissions_subregion %>%
        filter(Scenario %in% temp_compared_scenario, TechSensitivity %in% interested_sensitivity)
      ggplot()+
        geom_point(data = emissions_subregion_comparison,
                   aes(x = year, y = `Emissions (Mtons)`, color = `TechSensitivity`)) +
        scale_color_brewer(palette = "Set1")+
        geom_hline(yintercept = reference_emissions)+
        theme_bw()+
        theme(legend.position = "bottom",
              panel.grid.minor.x = element_blank()
        )+
        facet_grid(.~Scenario) + 
        labs(caption = "Horizontal lines show 2019 (simulated) emission level")+
        guides(color = guide_legend(nrow = 2)) + 
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Emission_',comparison[j],'_',temp_total_title,'Absolute_Emission.png'),width = 7.5,height=7)
      
      ggplot()+
        geom_point(data = emissions_subregion_comparison,
                   aes(x = year, y = `Generation Emissions Rate (Ton/MWh)`, color = `TechSensitivity`)) +
        geom_hline(yintercept = reference_GER)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        theme(legend.position = "bottom",
              panel.grid.minor.x = element_blank()
        )+
        facet_grid(.~Scenario) + 
        labs(caption = "Horizontal lines show 2019 (simulated) emission level") +
        guides(color = guide_legend(nrow = 2)) + 
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Emission_',comparison[j],'_',temp_total_title,'Gen_ER.png'),width = 7.5,height=7)
      
      ggplot()+
        geom_point(data = emissions_subregion_comparison,
                   aes(x = year, y = `Load Emissions Rate (Ton/MWh)`, color = `TechSensitivity`)) +

        geom_hline(yintercept = reference_LER)+
        scale_color_brewer(palette = "Set1")+
        theme_bw()+
        theme(legend.position = "bottom",
              panel.grid.minor.x = element_blank()
        )+
        facet_grid(.~Scenario) + 
        labs(caption = "Horizontal lines show 2019 (simulated) emission level")+
        guides(color = guide_legend(nrow = 2)) + 
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Emission_',comparison[j],'_',temp_total_title,'Load_ER.png'),width = 7.5,height=7)
      
    }
  }
}