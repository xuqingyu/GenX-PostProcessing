# Capacity Plot ----
source('./code/Header.R')
p_width = 12
p_height = 7
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_output_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Output_',temp_total_title,".csv")
  
  if (file.exists(gen_output_subregion_fn)){
    gen_output_subregion <- read_csv(gen_output_subregion_fn) %>%
      select(case,year,Fuel,Scenario, TechSensitivity, AnnualOutput) %>%
      filter(!(Fuel %in% storage_fuel), AnnualOutput > 1000)   %>%
      mutate(`AnnualOutput (TWh)` = AnnualOutput/1000000) %>%
      select(-AnnualOutput)
    gen_output_subregion$Scenario <- factor(gen_output_subregion$Scenario, levels = scenario)
    gen_output_subregion$TechSensitivity <- factor(gen_output_subregion$TechSensitivity, levels = tech_sensitivity)
    gen_output_subregion$Fuel <- factor(gen_output_subregion$Fuel, capacity_resource_levels);
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      gen_output_subregion_comparison <- gen_output_subregion %>%
        filter(Scenario %in% temp_compared_scenario);
      temp_techsensitivity_list <- interested_sensitivity
      for (k in 1:length(tech_sensitivity)){
        temp_data <- filter(gen_output_subregion_comparison, TechSensitivity == temp_techsensitivity_list[k])
        if (dim(temp_data)[1]!=0){
          ggplot()+
            geom_col(data = temp_data,
                     aes(x = year, y = `AnnualOutput (TWh)`, fill=Fuel),colour="black",size= 0.1) +
            scale_fill_manual(name = "Resources", values = fuel_colors) + 
            geom_hline(yintercept=0) + 
            facet_grid(.~Scenario) + 
            theme_bw()+
            theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank()
            )+ 
            facet_grid(.~Scenario) + 
            ggtitle(paste0('Generation Output of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
            ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenOutPut/GenOutput_',comparison[j],'_',temp_total_title,'_Sensitivity_',k,'.png'),
                   width = p_width,
                   height=p_height)
        }
      }
    }
    for (j in 1:n_sensitivity_comparison) {
      temp_sensitivity_comparison <- sensitivity_comparison[j]
      temp_target_sensitivity <- sensitivity_comparison_target$TechSensitivity_Comparison_Target[sensitivity_comparison_target$TechSensitivity_Comparison == temp_sensitivity_comparison]
      temp_compared_sensitivity <- sensitivity_comparison_sensitivity$TechSensitivity_Comparison_Sensitivity[sensitivity_comparison_sensitivity$TechSensitivity_Comparison == temp_sensitivity_comparison]
      n_compared_sensitivity <- length(temp_compared_sensitivity)
      # Build a template
      temp_fuel <- select(gen_output_subregion,Fuel) %>% distinct() %>% filter(!(Fuel %in% storage_fuel))
      temp_year_scenario_tech <- select(gen_output_subregion, year, Scenario, TechSensitivity) %>% filter(TechSensitivity %in% temp_compared_sensitivity) %>% distinct()
      template <- crossing(temp_fuel, temp_year_scenario_tech)
      gen_output_target_data <- gen_output_subregion %>%
        filter(TechSensitivity == temp_target_sensitivity) %>%
        rename(AnnualOutput_Target = `AnnualOutput (TWh)`) %>%
        select(-c(TechSensitivity,case));
      gen_output_compared_plot <- left_join(template,gen_output_target_data)
      
      gen_output_compared_data <- gen_output_subregion %>%
        filter(TechSensitivity %in% temp_compared_sensitivity) %>%
        select(-case);
      gen_output_compared_plot <- left_join(gen_output_compared_plot, gen_output_compared_data)
      
      gen_output_compared_plot[is.na(gen_output_compared_plot)] <- 0
      gen_output_compared_plot <- gen_output_compared_plot%>%
        mutate(`Difference (TWh)` = `AnnualOutput (TWh)` - AnnualOutput_Target);
      gen_output_target_data <- gen_output_target_data %>%
        rename(`AnnualOutput (TWh)` = AnnualOutput_Target) %>%
        mutate(TechSensitivity = temp_target_sensitivity)
      
      for (k in 1:length(interested_scenario)){
        temp_gen_output_target_data <- gen_output_target_data %>%
          filter(Scenario == interested_scenario[k]) 
        temp_gen_output_compared_data <- gen_output_compared_plot %>%
          filter(Scenario == interested_scenario[k])
        if (dim(temp_gen_output_target_data)[1]>0){
          g1 <- ggplot()+
            geom_col(data = temp_gen_output_target_data,
                     aes(x = year, y = `AnnualOutput (TWh)`, fill=Fuel),colour="black",size= 0.1) +
            scale_fill_manual(name = "Resources", values = fuel_colors) + 
            geom_hline(yintercept=0) + 
            theme_bw()+
            theme(legend.position = "none",
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank()
            )+
            facet_wrap(Scenario ~ TechSensitivity)
          g2 <- ggplot()+
            geom_col(data = temp_gen_output_compared_data,
                     aes(x = year, y = `Difference (TWh)`, fill=Fuel),colour="black",size= 0.1) +
            scale_fill_manual(name = "Resources", values = fuel_colors) + 
            geom_hline(yintercept=0) + 
            facet_grid(TechSensitivity~Scenario)+
            theme_bw()+
            theme(legend.position = "right",
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank()
            )
          
          ggdraw() +
            draw_plot(g1,x = 0, y = 0, width = .45) + 
            draw_plot(g2,x = 0.5, y = 0, width = .5) +
            ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenOutPut/Generation Output (TWh)_Sensitivity Comparison_', j ,"_of_", temp_total_title,"_of_", interested_scenario[k],'.png'),width = 9,height=7)
        }
      }
    }
  }
}
