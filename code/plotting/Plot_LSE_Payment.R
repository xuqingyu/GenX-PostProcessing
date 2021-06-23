# Plotting
source('./code/Header.R')
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_payment_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_long_',temp_total_title,"_with2019_and_DG.csv")
  if (file.exists(lse_payment_subregion_fn)){
    lse_payment_plot <- read_csv(lse_payment_subregion_fn)
    case_temp <- unique(select(lse_payment_plot,case))
    n_case_temp <- dim(case_temp)[1]
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      lse_payment_plot_comparison <- lse_payment_plot %>%
        mutate(`LSE Payment 2020US$/MWh` = value/`Gross Total`) %>%
        filter(Scenario %in% temp_compared_scenario)
      
      
      lse_payment_plot_comparison$Scenario <- factor(lse_payment_plot_comparison$Scenario, levels = scenario)
      lse_payment_plot_comparison$TechSensitivity <- factor(lse_payment_plot_comparison$TechSensitivity, levels = tech_sensitivity)
      lse_payment_plot_comparison$`Cost Type` = factor(lse_payment_plot_comparison$`Cost Type`, levels = load_cost_type)

      lse_payment_plot_total_withdg <- lse_payment_plot_comparison %>%
        group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
        summarize(value = sum(value)) %>%
        mutate(`LSE Payment 2020US$/MWh` = value/`Gross Total`)
      lse_payment_plot_total <- lse_payment_plot_comparison[lse_payment_plot_comparison$`Cost Type`!='NJ DG Cost',] %>%
        group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
        summarize(value = sum(value)) %>%
        mutate(`LSE Payment 2020US$/MWh` = value/`Gross Total`)
      
      ggplot()+
        geom_col(data = lse_payment_plot_comparison,
                 aes(x = year, y = `LSE Payment 2020US$/MWh`, fill = `Cost Type`),color='black',size=0.1) +
        scale_fill_manual(values = load_cost_color) +
        geom_line(data=lse_payment_plot_total_withdg, aes(x=year,y = `LSE Payment 2020US$/MWh`)) + 
        geom_point(data=lse_payment_plot_total_withdg, aes(x=year,y = `LSE Payment 2020US$/MWh`)) +
        facet_grid(`TechSensitivity`~Scenario) + 
        geom_hline(yintercept = 0)+
        labs(title = comparison[j]) +
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECostBreakDown_',comparison[j],'_',temp_total_title,'_with_NJ_DG.png'),width = 10,height=10)
      
      ggplot()+
        geom_col(data = lse_payment_plot_comparison,
                 aes(x = year, y = `value`, fill = `Cost Type`),color='black',size=0.1) +
        scale_fill_manual(values = load_cost_color) +
        geom_line(data=lse_payment_plot_total_withdg, aes(x=year,y = `value`)) + 
        geom_point(data=lse_payment_plot_total_withdg, aes(x=year,y = `value`)) +
        facet_grid(`TechSensitivity`~Scenario) + 
        geom_hline(yintercept = 0)+
        labs(title = comparison[j]) +
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECostBreakDown_total_',comparison[j],'_',temp_total_title,'_with_NJ_DG.png'),width = 10,height=10)
      
      ggplot()+
        geom_col(data = lse_payment_plot_comparison[lse_payment_plot_comparison$`Cost Type`!='NJ DG Cost',],
                 aes(x = year, y = `LSE Payment 2020US$/MWh`, fill = `Cost Type`),color='black',size=0.1) +
        scale_fill_manual(values = load_cost_color) +
        geom_line(data=lse_payment_plot_total, aes(x=year,y = `LSE Payment 2020US$/MWh`)) + 
        geom_point(data=lse_payment_plot_total, aes(x=year,y = `LSE Payment 2020US$/MWh`)) +
        facet_grid(`TechSensitivity`~Scenario) + 
        geom_hline(yintercept = 0)+
        labs(title = comparison[j]) +
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECostBreakDown_',comparison[j],'_',temp_total_title,'.png'),width = 10,height=10)
      
      ggplot()+
        geom_col(data = lse_payment_plot_comparison[lse_payment_plot_comparison$`Cost Type`!='NJ DG Cost',],
                 aes(x = year, y = `value`, fill = `Cost Type`),color='black',size=0.1) +
        scale_fill_manual(values = load_cost_color) +
        geom_line(data=lse_payment_plot_total, aes(x=year,y = `value`)) + 
        geom_point(data=lse_payment_plot_total, aes(x=year,y = `value`)) +
        facet_grid(`TechSensitivity`~Scenario) + 
        geom_hline(yintercept = 0)+
        labs(title = comparison[j]) +
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECostBreakDown_total_',comparison[j],'_',temp_total_title,'.png'),width = 10,height=10)    
    }
    
  }
}
