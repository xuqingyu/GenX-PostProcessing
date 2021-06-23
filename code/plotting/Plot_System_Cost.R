source('./code/Header.R')
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  system_cost_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/System_Cost_long_',temp_total_title,".csv")
  if (file.exists(system_cost_subregion_fn)){
    system_cost_plot <- read_csv(system_cost_subregion_fn)
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      system_cost_plot_comparison <- system_cost_plot %>%
        filter(Scenario %in% temp_compared_scenario) %>%
        mutate(`System Cost 2020US$/MWh` = value/`Gross Total`)
      
      
      system_cost_plot_comparison$Scenario <- factor(system_cost_plot_comparison$Scenario, levels = scenario)
      system_cost_plot_comparison$TechSensitivity <- factor(system_cost_plot_comparison$TechSensitivity, levels = tech_sensitivity)
      system_cost_plot_comparison$`Cost Type` = factor(system_cost_plot_comparison$`Cost Type`, levels = system_cost_type)
    
      system_cost_plot_total_withdg <- system_cost_plot_comparison %>%
        group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
        summarize(value = sum(value)) %>%
        mutate(`System Cost 2020US$/MWh` = value/`Gross Total`)
      system_cost_plot_total <- system_cost_plot_comparison[system_cost_plot_comparison$`Cost Type`!='NJ DG Cost',] %>%
        group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
        summarize(value = sum(value)) %>%
        mutate(`System Cost 2020US$/MWh` = value/`Gross Total`)
      
      ggplot()+
        geom_col(data = system_cost_plot_comparison,
                 aes(x = year, y = `System Cost 2020US$/MWh`, fill = `Cost Type`),color = 'black',size=0.1) +
        scale_fill_manual(values = system_cost_color) +
        geom_line(data=system_cost_plot_total_withdg, aes(x=year,y = `System Cost 2020US$/MWh`)) + 
        geom_point(data=system_cost_plot_total_withdg, aes(x=year,y = `System Cost 2020US$/MWh`)) +
        geom_hline(yintercept = 0)+
        facet_grid(`TechSensitivity`~Scenario) + 
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemCostBreakDown_',temp_total_title,"_",comparison[j],'_with_NJ_DG.png'),width = 10,height=10)
      
      ggplot()+
        geom_col(data = system_cost_plot_comparison,
                 aes(x = year, y = `value`, fill = `Cost Type`),color = 'black',size=0.1) +
        scale_fill_manual(values = system_cost_color) +
        geom_line(data=system_cost_plot_total_withdg, aes(x=year,y = `value`)) + 
        geom_point(data=system_cost_plot_total_withdg, aes(x=year,y = `value`)) +
        geom_hline(yintercept = 0)+
        facet_grid(`TechSensitivity`~Scenario) + 
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemCostBreakDown_total_',temp_total_title,"_",comparison[j],'_with_NJ_DG.png'),width = 10,height=10)
      
      ggplot()+
        geom_col(data = system_cost_plot_comparison[system_cost_plot_comparison$`Cost Type`!='NJ DG Cost',],
                 aes(x = year, y = `System Cost 2020US$/MWh`, fill = `Cost Type`),color = 'black',size=0.1) +
        scale_fill_manual(values = system_cost_color) +
        geom_line(data=system_cost_plot_total, aes(x=year,y = `System Cost 2020US$/MWh`)) + 
        geom_point(data=system_cost_plot_total, aes(x=year,y = `System Cost 2020US$/MWh`)) +
        geom_hline(yintercept = 0)+
        facet_grid(`TechSensitivity`~Scenario) + 
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemCostBreakDown_',temp_total_title,"_",comparison[j],'.png'),width = 10,height=10)
      
      ggplot()+
        geom_col(data = system_cost_plot_comparison[system_cost_plot_comparison$`Cost Type`!='NJ DG Cost',],
                 aes(x = year, y = `value`, fill = `Cost Type`),color = 'black',size=0.1) +
        scale_fill_manual(values = system_cost_color) +
        geom_line(data=system_cost_plot_total, aes(x=year,y = `value`)) + 
        geom_point(data=system_cost_plot_total, aes(x=year,y = `value`)) +
        geom_hline(yintercept = 0)+
        facet_grid(`TechSensitivity`~Scenario) + 
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemCostBreakDown_total_',temp_total_title,"_",comparison[j],'.png'),width = 10,height=10)   
    }
  }
}