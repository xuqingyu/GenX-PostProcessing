# Capacity Plot ----
source('./code/Header.R')
p_width = 10
p_height = 7
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_output_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Output_',temp_total_title,".csv")
  scenario_list = c('Current Policy (CP)',
                    'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                    'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                    'Deep Decarbonization (DD)', 'DD + High Solar', 'DD + 80% Instate', 'DD + High Solar + 80% Instate', 
                    'DD + Nuclear',  'DD + High Solar + Nuclear',  'DD + Nuclear + 80% Instate', 'DD + High Solar + Nuclear + 80% Instate')
  temp_load_combined <- read_csv(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Load/Load_Component_",Subregions[i],".csv")) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
  temp_load_bar_total <- temp_load_combined %>%
    filter(`Load Type` == 'Gross Total')
  
  temp_load_bar_total_2019 = temp_load_bar_total %>%
    mutate(year = 2019)
  if (temp_total_title == 'PJM'){
    temp_load_bar_total_2019$TWh = 800
  } else if (temp_total_title == 'New Jersey') {
    temp_load_bar_total_2019$TWh = 83
  }
  temp_load_bar_total = rbind(temp_load_bar_total, temp_load_bar_total_2019)
  if (file.exists(gen_output_subregion_fn)){
    gen_output_subregion <- read_csv(gen_output_subregion_fn) %>%
      select(case,year,Fuel,Scenario, TechSensitivity, AnnualOutput) %>%
      filter(!(Fuel %in% storage_fuel), AnnualOutput > 1000)   %>%
      mutate(`AnnualOutput (TWh)` = AnnualOutput/1000000) %>%
      select(-AnnualOutput) %>%
      mutate(Scenario = factor(Scenario, levels = scenario_list),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, capacity_resource_levels)) %>%
      arrange(Fuel)
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      gen_output_subregion_comparison <- gen_output_subregion %>%
        filter(Scenario %in% temp_compared_scenario) %>%
        group_by(case,year,Scenario,TechSensitivity)%>%
        mutate(outputlabel = round(`AnnualOutput (TWh)`,1))%>%
        mutate(pos = rev(cumsum(rev(outputlabel))) - (0.5 * outputlabel))
      gen_output_subregion_comparison$outputlabel[which(gen_output_subregion_comparison$outputlabel<1)] <-''
      
      temp_techsensitivity_list <- interested_sensitivity
      temp_load_bar = temp_load_bar_total %>%
        filter(Scenario %in% temp_compared_scenario)
      if(length(temp_compared_scenario) >4) {plot_row = 2} else {plot_row = 1}
      for (k in 1:length(tech_sensitivity)){
        temp_data <- filter(gen_output_subregion_comparison, TechSensitivity == temp_techsensitivity_list[k])
        temp_data_total <- temp_data %>%
          group_by(case, year, Scenario, TechSensitivity) %>%
          summarise(`Total AnnualOutput (TWh)` = sum(`AnnualOutput (TWh)`))
        temp_load <- filter(temp_load_bar, TechSensitivity == temp_techsensitivity_list[k]) %>%
          left_join(temp_data_total, by = c('case', 'year','Scenario','TechSensitivity'))
        if (dim(temp_data)[1]!=0){
          ggplot()+
            geom_col(data = temp_data,
                     aes(x = as.character(year), y = `AnnualOutput (TWh)`, fill=Fuel),colour="black",size= 0.1) +
            geom_text(data = temp_data,
                      aes(x=as.character(year), y = pos, label = formatC(outputlabel,format="f", digits = 1)), 
                      size = 3)+
            geom_errorbar(data = temp_load, 
                          aes(x = as.character(year), ymax = `TWh`,ymin = `Total AnnualOutput (TWh)`),
                          width = 0.5, size = 0.5, color = 'black')+
            geom_text(data = temp_load,
                       aes(x = as.character(year), 
                           y = `TWh`,
                           label = paste0('Demand = \n', formatC(`TWh`,format="f", digits = 1))),
                      nudge_y = 8,
                      size = 2,
                      fontface = "italic")+
            scale_fill_manual(name = "Resources", values = fuel_colors) + 
            geom_hline(yintercept=0) + 
            facet_grid(.~Scenario) + 
            theme_bw()+
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              legend.position = 'bottom'
            )+ 
            xlab('Year')+
            facet_wrap(.~Scenario, nrow = plot_row) + 
            guides(fill = guide_legend(nrow = 2, title.position = "top"))+
            ggtitle(paste0('Generation Output of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
            ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenOutPut/GenOutput_',comparison[j],'_',temp_total_title,'_Sensitivity_',k,'.eps'),
                   width  = p_width,
                   height = p_height,
                   device = "eps")
        }
      }
    }
  }
}
