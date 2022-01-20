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
      mutate(TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             `Revenue/Cost Type` = factor(`Revenue/Cost Type`, levels = genprofit_type)) 
    Policy = as.character(genprofit_subregion_plot$Scenario)
    Policy[grep('Clean Energy Standard',genprofit_subregion_plot$Scenario)] = 
      'Clean Energy Standard';
    Policy[grep('Cap-and-Trade',genprofit_subregion_plot$Scenario)] = 
      'Carbon Cap-and-Trade';
    
    gen_profit_subregion_plot_w_policy = cbind(genprofit_subregion_plot, Policy) %>%
      filter(year == '2030',
             TechSensitivity %in% MajorTechSensitivity) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity)) %>%
      filter(Policy %in%  c('Clean Energy Standard', 'Carbon Cap-and-Trade'))    
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (80% Reduction Compare to 2005 Level)'] = '80% Reduction'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (90% Reduction Compare to 2005 Level)'] = '90% Reduction'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (75% Reduction Compare to 2005 Level)'] = '75% Reduction'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (70% Reduction Compare to 2005 Level)'] = '70% Reduction'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (60% Reduction Compare to 2005 Level)'] = '60% Reduction'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (50% Reduction Compare to 2005 Level)'] = '50% Reduction'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (65%)'] = 'CES = 65%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (70%)'] = 'CES = 70%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (75%)'] = 'CES = 75%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (80%)'] = 'CES = 80%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (85%)'] = 'CES = 85%'        

    for (k in 1:length(MajorTechSensitivity)) {
      for (w in c('Clean Energy Standard','Carbon Cap-and-Trade')){
          genprofit_subregion_plot_comparison <- gen_profit_subregion_plot_w_policy %>%
            filter(TechSensitivity == MajorTechSensitivity[k],
                   Policy == w) %>%
            mutate(`Revenue/Cost 2020$/MWh` = round(value/AnnualOutput,2)) 
          
          genprofit_subregion_plot_total <- genprofit_subregion_plot_comparison %>%
            group_by(case,year,Fuel,Scenario,TechSensitivity,AnnualOutput,Policy) %>%
            summarize(value = sum(value)) %>%
            mutate(`Revenue/Cost 2020$/MWh` = round(value/AnnualOutput,2))
        for (j in c('Gas CC', 'Nuclear','Utility Solar', 'Onshore Wind')){
          ggplot()+
            geom_col(data = filter(genprofit_subregion_plot_comparison, 
                                   Fuel == j),
                     aes(x = Scenario, 
                         y = `Revenue/Cost 2020$/MWh`, 
                         fill = `Revenue/Cost Type`),
                     color='black',
                     size=0.1) +
            scale_fill_manual(values = genprofit_color) +
            geom_point(data=filter(genprofit_subregion_plot_total,
                                   Fuel == j), 
                       aes(x = Scenario, 
                           y = `Revenue/Cost 2020$/MWh`)) +
            geom_text(data=filter(genprofit_subregion_plot_total,
                                  Fuel == j), 
                      aes(x= Scenario, 
                          y = `Revenue/Cost 2020$/MWh`,
                          label = `Revenue/Cost 2020$/MWh`),
                      nudge_y = 3)+
            geom_hline(yintercept = 0)+
            coord_cartesian(ylim = c(-120,110))+
            scale_y_continuous(breaks = seq(-120,120,20))+
            facet_grid(.~Policy) + 
            theme_classic2()+
            theme(legend.position = "bottom")+
            labs(caption = paste0(j,' ', MajorTechSensitivity[k]))+
            xlab('')+
            guides(fill = guide_legend(nrow = 3, title.position = "top"))+
            # ggtitle(label = paste0('Generation Profit of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k],' of technology ', list_of_tech[w]))+
            ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/',j,'_ProfitBreakDown_',temp_total_title,'_',k,'_',w,'.png'),
                   width = 7,
                   height = p_height)
          
        }
      }
    }
  }
}
