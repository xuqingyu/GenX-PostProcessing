source('./code/Header.R')
p_width = 12
p_height = 7
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
    gen_capacity_subregion_fn <- paste0(RunFdr,'/CompiledResults/',
                                        Subregions[i],'/Generation/Gen_Capacity_w_2019',
                                        temp_total_title,".csv")
  } else {
    gen_capacity_subregion_fn <- paste0(RunFdr,'/CompiledResults/',
                                        Subregions[i],'/Generation/Gen_Capacity_',
                                        temp_total_title,".csv")
  }
  
  if (file.exists(gen_capacity_subregion_fn)){
    gen_capacity_subregion_plot_all <- read_csv(gen_capacity_subregion_fn, col_types = cols()) %>%
      pivot_longer(!c(case,year,Fuel,Scenario, TechSensitivity),names_to = 'Capacity Type') %>%
      filter(`Capacity Type` == 'Capacity') %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, levels = capacity_resource_levels),
             `Capacity (GW)` = value/1000,
             year = factor(year, levels = c(2019,2020,years))) %>%
      select(-c(value,`Capacity Type`))
    limits <- gen_capacity_subregion_plot_all %>%
      group_by(Scenario,TechSensitivity,year) %>%
      summarize(maxcapacity = sum(`Capacity (GW)`))
    limits <- max(limits$maxcapacity)
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      for (k in 1:length(interested_sensitivity)) {
        gen_capacity_subregion_plot <- gen_capacity_subregion_plot_all %>%
          filter(Scenario %in% temp_compared_scenario,
                 TechSensitivity == interested_sensitivity[k]);
        ggplot()+
          geom_col(data = filter(gen_capacity_subregion_plot),
                   aes(x = as.character(year), y = `Capacity (GW)`, 
                       fill=factor(Fuel, levels = capacity_resource_levels)),
                   colour="black", size= 0.1 ) +
          scale_fill_manual(name = "Resources", values = fuel_colors) + 
          # scale_y_continuous(breaks = seq(from = 0, to = limits, by = limits/10))+
          geom_hline(yintercept=0) + 
          facet_grid(.~Scenario) + 
          theme_bw()+
          # coord_cartesian(ylim = c(0, limits))+
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = 'bottom'
          )+
          xlab('Year')+
          # coord_cartesian(ylim=c(0,limits))+
          guides(fill = guide_legend(nrow = 3, title.position = "top"))+
          ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
          ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenCapacity/GenCapacity_',comparison[j],'_',temp_total_title,'_',k,'.png'),
                 width = p_width,
                 height = p_height)
      }
    }
    # for (j in 1:n_sensitivity_comparison) {
    #   temp_sensitivity_comparison <- sensitivity_comparison[j]
    #   temp_target_sensitivity <- sensitivity_comparison_target$TechSensitivity_Comparison_Target[sensitivity_comparison_target$TechSensitivity_Comparison == temp_sensitivity_comparison]
    #   temp_compared_sensitivity <- sensitivity_comparison_sensitivity$TechSensitivity_Comparison_Sensitivity[sensitivity_comparison_sensitivity$TechSensitivity_Comparison == temp_sensitivity_comparison]
    #   n_compared_sensitivity <- length(temp_compared_sensitivity)
    #   # Build a template
    #   temp_fuel <- select(gen_capacity_subregion_plot_all,Fuel) %>% distinct()
    #   temp_year_scenario_tech <- select(gen_capacity_subregion_plot_all, year, Scenario, TechSensitivity) %>% filter(TechSensitivity %in% temp_compared_sensitivity) %>% distinct()
    #   template <- crossing(temp_fuel, temp_year_scenario_tech)
    #   gen_capacity_target_data <- gen_capacity_subregion_plot_all %>%
    #     filter(TechSensitivity == temp_target_sensitivity) %>%
    #     rename(Capacity_Target = `Capacity (GW)`) %>%
    #     select(-c(TechSensitivity,case));
    #   gen_capacity_compared_plot <- left_join(template,gen_capacity_target_data)
    #   
    #   gen_capacity_compared_data <- gen_capacity_subregion_plot_all %>%
    #     filter(TechSensitivity %in% temp_compared_sensitivity) %>%
    #     select(-case);
    #   gen_capacity_compared_plot <- left_join(gen_capacity_compared_plot, gen_capacity_compared_data)
    #   
    #   gen_capacity_compared_plot[is.na(gen_capacity_compared_plot)] <- 0
    #   gen_capacity_compared_plot <- gen_capacity_compared_plot%>%
    #     mutate(`Difference (GW)` = `Capacity (GW)` - Capacity_Target);
    #   gen_capacity_target_data <- gen_capacity_target_data %>%
    #     rename(`Capacity (GW)` = Capacity_Target) %>%
    #     mutate(TechSensitivity = temp_target_sensitivity)
    #   
    #   for (k in 1:length(interested_scenario)){
    #     temp_gen_capacity_target_data <- gen_capacity_target_data %>%
    #       filter(Scenario == interested_scenario[k]) 
    #     temp_gen_capacity_compared_data <- gen_capacity_compared_plot %>%
    #       filter(Scenario == interested_scenario[k])
    #     if (dim(temp_gen_capacity_compared_data)[1]>0){
    #       g1 <- ggplot()+
    #         geom_col(data = temp_gen_capacity_target_data,
    #                  aes(x = year, y = `Capacity (GW)`, fill=Fuel),colour="black",size= 0.1) +
    #         scale_fill_manual(name = "Resources", values = fuel_colors) + 
    #         geom_hline(yintercept=0) + 
    #         theme_bw()+
    #         theme(legend.position = "none",
    #               panel.grid.major.x = element_blank(),
    #               panel.grid.minor.x = element_blank()
    #         )+
    #         facet_wrap(Scenario ~ TechSensitivity)
    #       g2 <- ggplot()+
    #         geom_col(data = temp_gen_capacity_compared_data,
    #                  aes(x = year, y = `Difference (GW)`, fill=Fuel),colour="black",size= 0.1) +
    #         scale_fill_manual(name = "Resources", values = fuel_colors) + 
    #         geom_hline(yintercept=0) + 
    #         facet_grid(TechSensitivity~Scenario)+
    #         theme_bw()+
    #         theme(legend.position = "right",
    #               panel.grid.major.x = element_blank(),
    #               panel.grid.minor.x = element_blank()
    #         )
    #       
    #       ggdraw() +
    #         draw_plot(g1,x = 0, y = 0, width = .45) + 
    #         draw_plot(g2,x = 0.5, y = 0, width = .5) +
    #         ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
    #                       '/Graphics/GenCapacity/Generation Capacity (GW)_Sensitivity Comparison_', j ,
    #                       "_of_", temp_total_title,"_of_", interested_scenario[k],'.png'),
    #                width = p_width,
    #                height= p_height)
    #     }
    #   }
    # }
  }
}
