source('./code/Header.R')
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
    gen_capacity_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Capacity_w_2019',temp_total_title,".csv")
  } else {
    gen_capacity_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Capacity',temp_total_title,".csv")
  }
  if (file.exists(gen_capacity_subregion_fn)){
    gen_capacity_subregion_plot_all <- read_csv(gen_capacity_subregion_fn) %>%
      pivot_longer(!c(case,year,Fuel,Scenario, TechSensitivity),names_to = 'Capacity Type')
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      gen_capacity_subregion_plot <- gen_capacity_subregion_plot_all %>%
        filter(Scenario %in% temp_compared_scenario);
      
      gen_capacity_subregion_plot <- gen_capacity_subregion_plot  %>%
        mutate(`Capacity (GW)` = value/1000)
      gen_capacity_subregion_plot$Scenario <- factor(gen_capacity_subregion_plot$Scenario, levels = scenario)
      gen_capacity_subregion_plot$TechSensitivity <- factor(gen_capacity_subregion_plot$TechSensitivity, levels = tech_sensitivity)
      gen_capacity_subregion_plot$Fuel <- factor(gen_capacity_subregion_plot$Fuel, levels = capacity_resource_levels)
      ggplot()+
        geom_col(data = filter(gen_capacity_subregion_plot,`Capacity Type` == 'Capacity'),
                 aes(x = year, y = `Capacity (GW)`, fill=factor(Fuel, levels = capacity_resource_levels)),colour="black",size= 0.1 ) +
        scale_fill_manual(name = "Resources", values = fuel_colors) + 
        geom_hline(yintercept=0) + 
        facet_grid(`TechSensitivity`~Scenario) + 
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenCapacity_',comparison[j],'_',temp_total_title,'.png'),width = 10,height=10)
    }
  }
}