p_width = 12
p_height = 7
settingfile <- 'sample_inputs_pjm.csv';
RunFdr <- '/Users/qingyuxu/Documents/PJM_QX_2022_PH1_newwacc'
source('./code/Header.R')
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_payment_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                     '/Load/LSE_Payment_long_',temp_total_title,"_with2019_and_DG.csv")
  if (file.exists(lse_payment_subregion_fn)){
    lse_payment_plot <- read_csv(lse_payment_subregion_fn) %>%
      mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1)) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             `Cost Type` =factor(`Cost Type`, levels = load_cost_type))
    Policy = as.character(lse_payment_plot$Scenario)
    Policy[grep('Clean Energy Standard',lse_payment_plot$Scenario)] = 
      'Clean Energy Standard';
    Policy[grep('Cap-and-Trade',lse_payment_plot$Scenario)] = 
      'Carbon Cap-and-Trade';
    
    
    
    lse_payment_plot_w_policy = cbind(lse_payment_plot, Policy) %>%
      filter(year == '2030',
             TechSensitivity == 'Mid',
             Policy %in%  c('Clean Energy Standard',
                            'Carbon Cap-and-Trade'))
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (80% Reduction Compare to 2005 Level)'] = '80% Reduction'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (90% Reduction Compare to 2005 Level)'] = '90% Reduction'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (75% Reduction Compare to 2005 Level)'] = '75% Reduction'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (70% Reduction Compare to 2005 Level)'] = '70% Reduction'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (60% Reduction Compare to 2005 Level)'] = '60% Reduction'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (50% Reduction Compare to 2005 Level)'] = '50% Reduction'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (65%)'] = 'CES = 65%'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (70%)'] = 'CES = 70%'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (75%)'] = 'CES = 75%'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (80%)'] = 'CES = 80%'
    lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (85%)'] = 'CES = 85%'
    
    lse_payment_plot_w_policy_2019 = cbind(lse_payment_plot, Policy) %>%
      filter(year == '2019',
             TechSensitivity == 'Mid',
             Scenario %in% c('Cap-and-Trade (80% Reduction Compare to 2005 Level)')) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity),
             Scenario = '2019 Reference',
             Policy = '2019 Reference')
    
    lse_payment_plot_total <- lse_payment_plot_w_policy[lse_payment_plot_w_policy$`Cost Type`!='NJ DG Cost',] %>%
      group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`,Policy) %>%
      summarize(value = sum(value)) %>%
      mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1))
    lse_payment_plot_total_2019 <- lse_payment_plot_w_policy_2019[lse_payment_plot_w_policy_2019$`Cost Type`!='NJ DG Cost',] %>%
      group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`,Policy) %>%
      summarize(value = sum(value)) %>%
      mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1))
    ggplot()+
      geom_col(data = lse_payment_plot_w_policy[lse_payment_plot_w_policy$`Cost Type`!='NJ DG Cost',],
               aes(x = Scenario, 
                   y = `LSE Payment 2020US$/MWh`, 
                   fill = `Cost Type`), 
               color='black',
               size=0.1) +
      scale_fill_manual(values = select(load_cost_color,-`NJ DG Cost`)) +
      geom_point(data=lse_payment_plot_total, 
                 aes(x= Scenario, 
                     y = `LSE Payment 2020US$/MWh`)) +
      geom_text(data=lse_payment_plot_total, 
                aes(x=Scenario, 
                    y = `LSE Payment 2020US$/MWh`,
                                                 label = `LSE Payment 2020US$/MWh`),nudge_y = 3)+
      geom_hline(yintercept = 0)+
      facet_grid(.~Policy, scale = 'free_x') + 
      scale_y_continuous(breaks = seq(from = -20, to = 70, by = 10))+
      coord_cartesian(ylim = c(-20, 70))+
      theme_bw()+
      theme(legend.position = "bottom")+
      guides(fill = guide_legend(nrow = 3, title.position = "left"))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECostBreakDown_capvsces.png'),
             width = p_width,
             height = p_height)
    ggplot()+
      geom_col(data = lse_payment_plot_w_policy_2019[lse_payment_plot_w_policy_2019$`Cost Type`!='NJ DG Cost',],
               aes(x = Scenario, y = `LSE Payment 2020US$/MWh`, fill = `Cost Type`), color='black',size=0.1) +
      scale_fill_manual(values = select(load_cost_color,-`NJ DG Cost`)) +
      geom_point(data=lse_payment_plot_total_2019, aes(x=Scenario,y = `LSE Payment 2020US$/MWh`)) +
      geom_text(data=lse_payment_plot_total_2019, aes(x=Scenario,y = `LSE Payment 2020US$/MWh`,
                                                 label = `LSE Payment 2020US$/MWh`),nudge_y = 3)+
      geom_hline(yintercept = 0)+
      facet_grid(.~Policy, scale = 'free_x') +  
      scale_y_continuous(breaks = seq(from = -20, to = 70, by = 10))+
      coord_cartesian(ylim = c(-20, 70))+
      theme_bw()+
      theme(legend.position = "bottom")+
      guides(fill = guide_legend(nrow = 3, title.position = "left"))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECostBreakDown_capvsces_2019.png'),
             width = 2,
             height = p_height)
    
    
  }
}
