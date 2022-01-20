# Plotting
source('./code/Header.R')
p_width = 10
p_height = 7
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  genprofit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Profit_',temp_total_title,'.csv')
  if (file.exists(genprofit_subregion_fn)){
    genprofit_subregion_plot <- read_csv(genprofit_subregion_fn) %>%
      select(-c(AnnualCharge,Capacity,`Energy Capacity`)) %>%
      filter(!(Fuel %in% storage_fuel),
             AnnualOutput >= 1000) %>%
      pivot_longer(cols = !c(case,year,Fuel,Scenario,TechSensitivity,AnnualOutput),names_to = 'Revenue/Cost Type') %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             `Revenue/Cost Type` = factor(`Revenue/Cost Type`, levels = genprofit_type)) 
    case_temp <- unique(select(lse_payment_plot,case))
    n_case_temp <- dim(case_temp)[1]
    for (j in 1:n_comparison){
      for (k in 1:length(interested_sensitivity)){
        temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
        genprofit_subregion_plot_comparison <- genprofit_subregion_plot %>%
          filter(Scenario %in% temp_compared_scenario, 
                 TechSensitivity == interested_sensitivity[k]) %>%
          mutate(`Revenue/Cost 2020$/MWh` = round(value/AnnualOutput,2)) 
        
        genprofit_subregion_plot_total <- genprofit_subregion_plot_comparison %>%
          group_by(case,year,Fuel,Scenario,TechSensitivity,AnnualOutput) %>%
          summarize(value = sum(value)) %>%
          mutate(`Revenue/Cost 2020$/MWh` = round(value/AnnualOutput,2))
        list_of_tech = genprofit_subregion_plot_total$Fuel %>% unique()
        for (w in 1:length(list_of_tech)){
          
          ggplot()+
            geom_col(data = filter(genprofit_subregion_plot_comparison,Fuel == list_of_tech[w]),
                     aes(x = year, y = `Revenue/Cost 2020$/MWh`, fill = `Revenue/Cost Type`),color='black',size=0.1) +
            scale_fill_manual(values = genprofit_color) +
            geom_line(data=filter(genprofit_subregion_plot_total,Fuel == list_of_tech[w]), aes(x=year,y = `Revenue/Cost 2020$/MWh`)) + 
            geom_point(data=filter(genprofit_subregion_plot_total,Fuel == list_of_tech[w]), aes(x=year,y = `Revenue/Cost 2020$/MWh`)) +
            geom_text(data=filter(genprofit_subregion_plot_total,Fuel == list_of_tech[w]), aes(x=year,y = `Revenue/Cost 2020$/MWh`,label = `Revenue/Cost 2020$/MWh`),nudge_y = 3)+
            geom_hline(yintercept = 0)+
            facet_grid(.~Scenario) + 
            theme_bw()+
            theme(legend.position = "bottom")+
            guides(fill = guide_legend(nrow = 3, title.position = "left"))+
            ggtitle(label = paste0('Generation Profit of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k],' of technology ', list_of_tech[w]))+
            ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenProfit/GenProfitBreakDown_',comparison[j],'_',temp_total_title,'_',k,'_',w,'_with_NJ_DG.png'),
                   width = p_width,
                   height = p_height)
          
        }
        
      }
      
    }
    
  }
}
