settingfile <- 'postprocessing_inputs.csv';
RunFdr <-"/Users/qingyuxu/Documents/pjm_ce_all/"
source('./code/Header.R')
p_width = 12
p_height = 7
ScenarioFilter = c('Cap-and-Trade (40% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (45% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (100% Reduction Compare to 2005 Level)',
                   'Clean Energy Standard (40%)',
                   'Clean Energy Standard (45%)',
                   'Clean Energy Standard (100%)')
ylims = c(0,500)
ybreaks = seq(0,500,50)
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
    gen_capacity_subregion_fn <- paste0(RunFdr,'/CompiledResults/',
                                        Subregions[i],'/Generation/Gen_Capacity_',
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
    MajorTechSensitivity <- c('Mid',
                              'Low RE/BESS Cost', 
                              'Low NatGas Price', 
                              'High RE/BESS Cost',
                              'High NatGas Price',
                              "No Interregional Transmission Upgrade",
                              "No New Gas Installation", 
                              "Allow CCS Expansion", 
                              "New Gas Capacity Capped at 20% of Existing", 
                              "Half Interregional Transmission Upgrade", 
                              "No Nuclear Retirement")
    
    gen_capacity_subregion_plot_all <- read_csv(gen_capacity_subregion_fn, col_types = cols()) %>%
      pivot_longer(!c(case,year,Fuel,Scenario, TechSensitivity),names_to = 'Capacity Type') %>%
      filter(`Capacity Type` == 'Capacity') %>%
      filter(Fuel != 'Adv. Nuclear') %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, levels = capacity_resource_levels),
             `Capacity (GW)` = value/1000,
             year = factor(year, levels = c(2020,years))) %>%
      select(-c(value,`Capacity Type`)) %>%
      arrange(Fuel)%>%
      group_by(case,year,Scenario,TechSensitivity)%>%
      mutate(capacitylabel = round(`Capacity (GW)`,1))%>%
      mutate(pos = rev(cumsum(rev(capacitylabel))) - (0.5 * capacitylabel)) %>%
      ungroup()
    gen_capacity_subregion_plot_all$capacitylabel = formatC( round(gen_capacity_subregion_plot_all$capacitylabel,1) ,format = 'f', digits = 1)
    gen_capacity_subregion_plot_all$capacitylabel[which(as.numeric(gen_capacity_subregion_plot_all$capacitylabel)<4.99)] <- ''
    Policy = as.character(gen_capacity_subregion_plot_all$Scenario)
    Policy[grep('Clean Energy Standard',gen_capacity_subregion_plot_all$Scenario)] = 'Clean Energy Standard';
    Policy[grep('Cap-and-Trade',gen_capacity_subregion_plot_all$Scenario)] = 'Carbon Cap-and-Trade';
    
    gen_capacity_subregion_plot_w_policy = cbind(gen_capacity_subregion_plot_all, Policy) %>%
      filter(year == '2030',
             TechSensitivity %in% MajorTechSensitivity) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity)) %>%
      filter(Policy %in%  c('Clean Energy Standard', 'Carbon Cap-and-Trade')) %>%
      filter(!(Scenario %in% ScenarioFilter))

      
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (80% Reduction Compare to 2005 Level)'] = '80%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (90% Reduction Compare to 2005 Level)'] = '90%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (75% Reduction Compare to 2005 Level)'] = '75%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (70% Reduction Compare to 2005 Level)'] = '70%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (60% Reduction Compare to 2005 Level)'] = '60%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (50% Reduction Compare to 2005 Level)'] = '50%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (55% Reduction Compare to 2005 Level)'] = '55%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (65% Reduction Compare to 2005 Level)'] = '65%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (85% Reduction Compare to 2005 Level)'] = '85%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (95% Reduction Compare to 2005 Level)'] = '95%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (65%)'] = '65%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (70%)'] = '70%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (75%)'] = '75%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (80%)'] = '80%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (85%)'] = '85%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (50%)'] = '50%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (55%)'] = '55%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (60%)'] = '60%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (90%)'] = '90%'
    gen_capacity_subregion_plot_w_policy$Scenario[gen_capacity_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (95%)'] = '95%'
    for (k in 1:length(MajorTechSensitivity)) {
      for (w in c(c('Clean Energy Standard','Carbon Cap-and-Trade'))){
        if (w == 'Clean Energy Standard') {
          xlabel = '% of Total Load Supported by Clean Energy '
        } else {
          xlabel = '% Emission Reduction Compared to the 2005 Level'
        }
        ggplot()+
          geom_col(data = filter(gen_capacity_subregion_plot_w_policy,
                                 TechSensitivity == MajorTechSensitivity[k],
                                 Policy == w),
                   aes(x = Scenario, 
                       y = `Capacity (GW)`, 
                       fill=factor(Fuel, levels = capacity_resource_levels)),
                   colour="black", size= 0.1 ) +
          geom_text(data = filter(gen_capacity_subregion_plot_w_policy,
                                   TechSensitivity == MajorTechSensitivity[k],
                                   Policy == w),
                     aes(x = Scenario, 
                         y = pos,
                         label = capacitylabel)) + 
          scale_fill_manual(name = "Resources", values = fuel_colors) + 
          scale_y_continuous(breaks = ybreaks)+
          geom_hline(yintercept=0) +  
          theme_classic2()+
          coord_cartesian(ylim = ylims)+
          theme(
            legend.position = c(0.4,0.9),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
          )+
          labs(caption = MajorTechSensitivity[k])+
          guides(fill = guide_legend(nrow = 4, title.position = 'top'))+
          xlab(xlabel)+
          # coord_cartesian(ylim=c(0,limits))+
          # ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
          ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenCapacity/GenCapacity_2030_',w,'_',k,'.png'),
                 width = 7,
                 height = p_height)
      }

    }
    
    
    gen_capacity_subregion_plot_2019 <- read_csv(gen_capacity_subregion_fn, col_types = cols()) %>%
      pivot_longer(!c(case,year,Fuel,Scenario, TechSensitivity),names_to = 'Capacity Type') %>%
      filter(`Capacity Type` == 'Capacity') %>%
      mutate(Scenario = factor(Scenario, levels = scenario),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, levels = capacity_resource_levels),
             `Capacity (GW)` = value/1000) %>%
      select(-c(value,`Capacity Type`)) %>%
      filter(year == '2021',
             TechSensitivity == 'Mid',
             Scenario %in% c('Cap-and-Trade (80% Reduction Compare to 2005 Level)')) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity),
             Scenario = '2021 Reference') %>%
      arrange(Fuel)%>%
      group_by(case,year,Scenario,TechSensitivity)%>%
      mutate(capacitylabel = round(`Capacity (GW)`,1))%>%
      mutate(pos = rev(cumsum(rev(capacitylabel))) - (0.5 * capacitylabel)) %>%
      unique()

    gen_capacity_subregion_plot_2019$capacitylabel = formatC(round(gen_capacity_subregion_plot_2019$capacitylabel,1),format = 'f', digits = 1)
    gen_capacity_subregion_plot_2019$capacitylabel[which(as.numeric(gen_capacity_subregion_plot_2019$capacitylabel)<4.99)] <- ''    
    ggplot()+
      geom_col(data = filter(gen_capacity_subregion_plot_2019),
               aes(x = Scenario, y = `Capacity (GW)`, 
                   fill=factor(Fuel, levels = capacity_resource_levels)),
               colour="black", size= 0.1 ) +
      geom_text(data = filter(gen_capacity_subregion_plot_2019),
                aes(x = Scenario, 
                    y = pos,
                    label = capacitylabel)) + 
      scale_fill_manual(name = "Resources", values = fuel_colors) + 
      scale_y_continuous(breaks = ybreaks)+
      geom_hline(yintercept=0) +  
      theme_classic2()+
      coord_cartesian(ylim = ylims)+
      theme(
        legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )+
      labs(caption = 'Mid')+
      guides(fill = guide_legend(nrow = 2, title.position = "top"))+
      xlab("")+
      # coord_cartesian(ylim=c(0,limits))+
      # ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenCapacity/GenCapacity_2019_capvsces.png'),
             width = 2,
             height = p_height)
    
    for (k in 1:length(MajorTechSensitivity)) {
      gen_capacity_subregion_plot_reference <- read_csv(gen_capacity_subregion_fn, col_types = cols()) %>%
        pivot_longer(!c(case,year,Fuel,Scenario, TechSensitivity),names_to = 'Capacity Type') %>%
        filter(`Capacity Type` == 'Capacity') %>%
        mutate(Scenario = factor(Scenario, levels = scenario),
               TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
               Fuel = factor(Fuel, levels = capacity_resource_levels),
               `Capacity (GW)` = value/1000) %>%
        select(-c(value,`Capacity Type`)) %>%
        filter(year == '2030',
               TechSensitivity == MajorTechSensitivity[k],
               Scenario == 'DD (No Policy Reference)') %>%
        mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity),
               Scenario = 'Current Policy Reference') %>%
        arrange(Fuel)%>%
        group_by(case,year,Scenario,TechSensitivity)%>%
        mutate(capacitylabel = round(`Capacity (GW)`,1))%>%
        mutate(pos = rev(cumsum(rev(capacitylabel))) - (0.5 * capacitylabel))
      gen_capacity_subregion_plot_reference$capacitylabel = formatC(round(gen_capacity_subregion_plot_reference$capacitylabel,1),format = 'f', digits = 1)
      gen_capacity_subregion_plot_reference$capacitylabel[which(as.numeric(gen_capacity_subregion_plot_reference$capacitylabel)<4.99)] <- ''          
      ggplot()+
        geom_col(data = filter(gen_capacity_subregion_plot_reference),
                 aes(x = Scenario, y = `Capacity (GW)`, 
                     fill=factor(Fuel, levels = capacity_resource_levels)),
                 colour="black", size= 0.1 ) +
        geom_text(data = filter(gen_capacity_subregion_plot_reference),
                  aes(x = Scenario, 
                      y = pos,
                      label = capacitylabel)) + 
        scale_fill_manual(name = "Resources", values = fuel_colors) + 
        scale_y_continuous(breaks = ybreaks)+
        geom_hline(yintercept=0) +  
        theme_classic2()+
        coord_cartesian(ylim = ylims)+
        theme(
          legend.position = 'none',
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()
        )+
        guides(fill = guide_legend(nrow = 2, title.position = "top"))+
        xlab("")+
        labs(caption = MajorTechSensitivity[k])+
        # coord_cartesian(ylim=c(0,limits))+
        # ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenCapacity/GenCapacity_2030_Reference_',k,'.png'),
               width = 2,
               height = p_height)
      
    }
  }
}
