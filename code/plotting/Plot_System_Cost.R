source('./code/Header.R')
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  system_cost_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/System_Cost_long_',temp_total_title,".csv")
  if (file.exists(system_cost_subregion_fn)){
    system_cost_plot <- read_csv(system_cost_subregion_fn) %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             `Cost Type` = factor(`Cost Type`, levels = system_cost_type),
             `System Cost 2020US$/MWh` = round(value/`Gross Total`,2))
    for (j in 1:n_comparison){
      for (k in 1:length(interested_sensitivity)){
        temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
        system_cost_plot_comparison <- system_cost_plot %>%
          filter(Scenario %in% temp_compared_scenario, 
                 TechSensitivity == interested_sensitivity[k])
        
        system_cost_plot_total_withdg <- system_cost_plot_comparison %>%
          group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
          summarize(value = sum(value)) %>%
          mutate(`System Cost 2020US$/MWh` = round(value/`Gross Total`,2))
        system_cost_plot_total <- system_cost_plot_comparison[system_cost_plot_comparison$`Cost Type`!='NJ DG Cost',] %>%
          group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
          summarize(value = sum(value)) %>%
          mutate(`System Cost 2020US$/MWh` = round(value/`Gross Total`,2))
        
        ggplot()+
          geom_col(data = system_cost_plot_comparison,
                   aes(x = year, y = `System Cost 2020US$/MWh`, fill = `Cost Type`),color = 'black',size=0.1) +
          scale_fill_manual(values = system_cost_color) +
          geom_line(data=system_cost_plot_total_withdg, aes(x=year,y = `System Cost 2020US$/MWh`)) + 
          geom_point(data=system_cost_plot_total_withdg, aes(x=year,y = `System Cost 2020US$/MWh`)) +
          geom_text(data=system_cost_plot_total_withdg, aes(x=year,y = `System Cost 2020US$/MWh`,label = `System Cost 2020US$/MWh`),nudge_y = 3)+
          geom_hline(yintercept = 0)+
          facet_grid(.~Scenario) + 
          theme_bw()+
          theme(legend.position = "bottom")+
          guides(fill = guide_legend(nrow = 3, title.position = "left"))+
          ggtitle(label = paste0('Regional Cost of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
          ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemCost/SystemCostBreakDown_',temp_total_title,"_",comparison[j],'_',k,'_with_NJ_DG.png'),width = 9,height=7)
        
        # ggplot()+
        #   geom_col(data = system_cost_plot_comparison,
        #            aes(x = year, y = `value`, fill = `Cost Type`),color = 'black',size=0.1) +
        #   scale_fill_manual(values = system_cost_color) +
        #   geom_line(data=system_cost_plot_total_withdg, aes(x=year,y = `value`)) + 
        #   geom_point(data=system_cost_plot_total_withdg, aes(x=year,y = `value`)) +
        #   geom_hline(yintercept = 0)+
        #   facet_grid(.~Scenario) + 
        #   theme_bw()+
        #   theme(legend.position = "bottom")+
        #   guides(fill = guide_legend(nrow = 3, title.position = "left"))+
        #   ggtitle(label = paste0('Regional Cost of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
        #   ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemCost/SystemCostBreakDown_total_',temp_total_title,"_",comparison[j],'_',k,'_with_NJ_DG.png'),width = 9,height=7)
        
        ggplot()+
          geom_col(data = system_cost_plot_comparison[system_cost_plot_comparison$`Cost Type`!='NJ DG Cost',],
                   aes(x = year, y = `System Cost 2020US$/MWh`, fill = `Cost Type`),color = 'black',size=0.1) +
          scale_fill_manual(values = select(system_cost_color,-`NJ DG Cost`)) +
          geom_line(data=system_cost_plot_total, aes(x=year,y = `System Cost 2020US$/MWh`)) + 
          geom_point(data=system_cost_plot_total, aes(x=year,y = `System Cost 2020US$/MWh`)) +
          geom_text(data=system_cost_plot_total, aes(x=year,y = `System Cost 2020US$/MWh`,label = `System Cost 2020US$/MWh`),nudge_y = 3)+
          geom_hline(yintercept = 0)+
          facet_grid(.~Scenario) + 
          theme_bw()+
          theme(legend.position = "bottom")+
          guides(fill = guide_legend(nrow = 3, title.position = "left"))+
          ggtitle(label = paste0('Regional Cost of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
          ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemCost/SystemCostBreakDown_',temp_total_title,"_",comparison[j],'_',k,'.png'),width = 9,height=7)
        
        # ggplot()+
        #   geom_col(data = system_cost_plot_comparison[system_cost_plot_comparison$`Cost Type`!='NJ DG Cost',],
        #            aes(x = year, y = `value`, fill = `Cost Type`),color = 'black',size=0.1) +
        #   scale_fill_manual(values = select(system_cost_color,-`NJ DG Cost`)) +
        #   geom_line(data=system_cost_plot_total, aes(x=year,y = `value`)) + 
        #   geom_point(data=system_cost_plot_total, aes(x=year,y = `value`)) +
        #   geom_hline(yintercept = 0)+
        #   theme_bw()+
        #   theme(legend.position = "bottom")+
        #   guides(fill = guide_legend(nrow = 3, title.position = "left"))+
        #   ggtitle(label = paste0('Regional Cost of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
        #   ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/SystemCost/SystemCostBreakDown_total_',temp_total_title,"_",comparison[j],'_',k,'.png'),width = 9,height=7)   
      }
    }
  }
}