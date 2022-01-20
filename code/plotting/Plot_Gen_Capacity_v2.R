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
    gen_output_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                      '/Generation/Gen_Output_',temp_total_title,".csv")
  } else {
    gen_capacity_subregion_fn <- paste0(RunFdr,'/CompiledResults/',
                                        Subregions[i],'/Generation/Gen_Capacity_',
                                        temp_total_title,".csv")
    gen_output_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                      '/Generation/Gen_Output_',temp_total_title,".csv")
  }
  
  if (file.exists(gen_capacity_subregion_fn)){
    MajorTechSensitivity <- c('Low RE/BESS Cost', 
                              'Low NatGas Price',
                              'Mid',
                              'High RE/BESS Cost',
                              'High NatGas Price')
    gen_capacity_subregion_plot_all <- read_csv(gen_capacity_subregion_fn, col_types = cols()) %>%
      pivot_longer(!c(case,year,Fuel,Scenario, TechSensitivity),names_to = 'Capacity Type') %>%
      filter(`Capacity Type` == 'Capacity') %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, levels = capacity_resource_levels),
             `Capacity (GW)` = value/1000,
             year = factor(year, levels = c(2020,years))) %>%
      select(-c(value,`Capacity Type`)) %>%
      filter(year == '2030',
             TechSensitivity %in% MajorTechSensitivity,
             Scenario %in% c('Cap-and-Trade (80% Reduction Compare to 2005 Level)',
                             'Clean Energy Standard (80%)')) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity))
    # limits <- gen_capacity_subregion_plot_all %>%
    #   group_by(Scenario,TechSensitivity,year) %>%
    #   summarize(maxcapacity = sum(`Capacity (GW)`))
    # limits <- max(limits$maxcapacity)
    # 
    ggplot()+
      geom_col(data = filter(gen_capacity_subregion_plot_all),
               aes(x = TechSensitivity, y = `Capacity (GW)`, 
                   fill=factor(Fuel, levels = capacity_resource_levels)),
               colour="black", size= 0.1 ) +
      scale_fill_manual(name = "Resources", values = fuel_colors) + 
      scale_y_continuous(breaks = seq(from = 0, to = 400, by = 50))+
      geom_hline(yintercept=0) + 
      facet_grid(.~Scenario) + 
      theme_bw()+
      coord_cartesian(ylim = c(0, 400))+
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )+
      theme(legend.position = 'bottom')+
      guides(fill = guide_legend(nrow = 2, title.position = "top"))+
      xlab("")+
      # coord_cartesian(ylim=c(0,limits))+
      # ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenCapacity_2030_capvsces.png'),
             width = p_width,
             height = p_height)
    gen_capacity_subregion_plot_2019 <- read_csv(gen_capacity_subregion_fn, col_types = cols()) %>%
      pivot_longer(!c(case,year,Fuel,Scenario, TechSensitivity),names_to = 'Capacity Type') %>%
      filter(`Capacity Type` == 'Capacity') %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, levels = capacity_resource_levels),
             `Capacity (GW)` = value/1000) %>%
      select(-c(value,`Capacity Type`)) %>%
      filter(year == '2019',
             TechSensitivity == 'Mid',
             Scenario %in% c('Cap-and-Trade (80% Reduction Compare to 2005 Level)')) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity),
             Scenario = '2019 Reference')
    ggplot()+
      geom_col(data = filter(gen_capacity_subregion_plot_2019),
               aes(x = TechSensitivity, y = `Capacity (GW)`, 
                   fill=factor(Fuel, levels = capacity_resource_levels)),
               colour="black", size= 0.1 ) +
      scale_fill_manual(name = "Resources", values = fuel_colors) + 
      scale_y_continuous(breaks = seq(from = 0, to = 400, by = 50))+
      geom_hline(yintercept=0) + 
      facet_grid(.~Scenario) + 
      theme_bw()+
      coord_cartesian(ylim = c(0, 400))+
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )+
      theme(legend.position = 'bottom')+
      guides(fill = guide_legend(nrow = 2, title.position = "top"))+
      xlab("")+
      # coord_cartesian(ylim=c(0,limits))+
      # ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenCapacity_2019_capvsces.png'),
             width = 2,
             height = p_height)    
    
    
    
    MajorTechSensitivity <- c('Low RE/BESS Cost', 
                              'Low NatGas Price',
                              'Mid',
                              'High RE/BESS Cost',
                              'High NatGas Price')
    gen_output_subregion_plot_all <- read_csv(gen_output_subregion_fn) %>%
      select(case,year,Fuel,Scenario, TechSensitivity, AnnualOutput) %>%
      filter(!(Fuel %in% storage_fuel), AnnualOutput > 1000)   %>%
      mutate(`AnnualOutput (TWh)` = AnnualOutput/1000000) %>%
      select(-AnnualOutput) %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, levels = capacity_resource_levels),
             year = factor(year, levels = c(2020,years))) %>%
      filter(year == '2030',
             TechSensitivity %in% MajorTechSensitivity,
             Scenario %in% c('Cap-and-Trade (80% Reduction Compare to 2005 Level)',
                             'Clean Energy Standard (80%)')) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity))
    
    
    ggplot()+
      geom_col(data = filter(gen_output_subregion_plot_all),
               aes(x = TechSensitivity, y = `AnnualOutput (TWh)`, 
                   fill=Fuel),
               colour="black", size= 0.1 ) +
      scale_fill_manual(name = "Resources", values = fuel_colors) + 
      scale_y_continuous(breaks = seq(from = 0, to = 1100, by = 100))+
      geom_hline(yintercept=0) + 
      geom_hline(yintercept=907) + 
      facet_grid(.~Scenario) + 
      theme_bw()+
      coord_cartesian(ylim = c(0, 1100))+
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )+
      theme(legend.position = 'bottom')+
      guides(fill = guide_legend(nrow = 2, title.position = "top"))+
      xlab("")+
      # coord_cartesian(ylim=c(0,limits))+
      # ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenOutput_2030_capvsces.png'),
             width = p_width,
             height = p_height)
    
    gen_output_subregion_plot_all_2019 <- read_csv(gen_output_subregion_fn) %>%
      select(case,year,Fuel,Scenario, TechSensitivity, AnnualOutput) %>%
      filter(!(Fuel %in% storage_fuel), AnnualOutput > 1000)   %>%
      mutate(`AnnualOutput (TWh)` = AnnualOutput/1000000) %>%
      select(-AnnualOutput) %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
      filter(year == '2019',
             TechSensitivity == 'Mid',
             Scenario %in% c('Cap-and-Trade (80% Reduction Compare to 2005 Level)')) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity),
             Scenario = '2019 Reference')
    
    ggplot()+
      geom_col(data = filter(gen_output_subregion_plot_all_2019),
               aes(x = TechSensitivity, y = `AnnualOutput (TWh)`, 
                   fill=Fuel),
               colour="black", size= 0.1 ) +
      scale_fill_manual(name = "Resources", values = fuel_colors) + 
      scale_y_continuous(breaks = seq(from = 0, to = 1100, by = 100))+
      geom_hline(yintercept=0) + 
      geom_hline(yintercept=799.6) + 
      facet_grid(.~Scenario) + 
      theme_bw()+
      coord_cartesian(ylim = c(0, 1100))+
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )+
      theme(legend.position = 'bottom')+
      guides(fill = guide_legend(nrow = 2, title.position = "top"))+
      xlab("")+
      # coord_cartesian(ylim=c(0,limits))+
      # ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenOutput_2019_capvsces.png'),
             width = 2,
             height = p_height)
  }
}
