p_width = 12
p_height = 7
settingfile <- 'postprocessing_inputs.csv';
RunFdr <-"/Users/qingyuxu/Documents/pjm_ce_all/"
source('./code/Header.R')
ScenarioFilter = c('Cap-and-Trade (40% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (45% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (100% Reduction Compare to 2005 Level)',
                   'Clean Energy Standard (40%)',
                   'Clean Energy Standard (45%)',
                   'Clean Energy Standard (100%)')
ylims = c(-90,170)
ybreaks = seq(-200,200,20)
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_payment_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                     '/Load/LSE_Payment_long_',temp_total_title,"_with2019_and_DG.csv")

  if (file.exists(lse_payment_subregion_fn)){
    lse_payment_plot <- read_csv(lse_payment_subregion_fn) %>%
      mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1)) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             `Cost Type` =factor(`Cost Type`, levels = load_cost_type))%>%
      arrange(`Cost Type`) %>%
      filter(!(Scenario %in% ScenarioFilter))
    lse_payment_plot_w_ce_emission <- lse_payment_plot 


    
    Policy = as.character(lse_payment_plot_w_ce_emission$Scenario)
    Policy[grep('Clean Energy Standard',lse_payment_plot_w_ce_emission$Scenario)] = 
      'Clean Energy Standard';
    Policy[grep('Cap-and-Trade',lse_payment_plot_w_ce_emission$Scenario)] = 
      'Carbon Cap-and-Trade';
    
    MajorTechSensitivity <- c('Mid',
                              'Low RE/BESS Cost', 
                              'Low NatGas Price', 
                              'High RE/BESS Cost',
                              'High NatGas Price',
                              'Allow CCS Expansion',
                              'Half Interregional Transmission Upgrade',
                              'No Interregional Transmission Upgrade',
                              'New Gas Capacity Capped at 20% of Existing',
                              'No New Gas Installation',
                              'No Nuclear Retirement')    
    for (k in 1:length(MajorTechSensitivity)) {
      lse_payment_plot_w_policy = cbind(lse_payment_plot_w_ce_emission, Policy) %>%
        filter(year == '2030',
               TechSensitivity == MajorTechSensitivity[k],
               Policy %in%  c('Clean Energy Standard',
                              'Carbon Cap-and-Trade'))%>% 
        mutate(cost_label = `LSE Payment 2020US$/MWh`) %>%
        filter(`Cost Type` != 'NJ DG Cost'); 
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (80% Reduction Compare to 2005 Level)'] = '80%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (90% Reduction Compare to 2005 Level)'] = '90%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (75% Reduction Compare to 2005 Level)'] = '75%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (70% Reduction Compare to 2005 Level)'] = '70%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (60% Reduction Compare to 2005 Level)'] = '60%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (50% Reduction Compare to 2005 Level)'] = '50%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (55% Reduction Compare to 2005 Level)'] = '55%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (65% Reduction Compare to 2005 Level)'] = '65%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (85% Reduction Compare to 2005 Level)'] = '85%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Cap-and-Trade (95% Reduction Compare to 2005 Level)'] = '95%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (65%)'] = '65%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (70%)'] = '70%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (75%)'] = '75%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (80%)'] = '80%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (85%)'] = '85%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (50%)'] = '50%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (55%)'] = '55%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (60%)'] = '60%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (90%)'] = '90%'
      lse_payment_plot_w_policy$Scenario[lse_payment_plot_w_policy$Scenario == 'Clean Energy Standard (95%)'] = '95%'

      # lse_payment_plot_total <- lse_payment_plot_w_policy[lse_payment_plot_w_policy$`Cost Type`!='NJ DG Cost',] %>%
      #   group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`,Policy,`Load Emissions Rate (Ton/MWh)`,CEshare) %>%
      #   summarize(value = sum(value)) %>%
      #   mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1))
      
      
      lse_payment_plot_w_policy_pos <- lse_payment_plot_w_policy %>%
        filter(`LSE Payment 2020US$/MWh` >=0)%>%
        group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
        mutate(pos = rev(cumsum(rev(`LSE Payment 2020US$/MWh`))) - (0.5 * `LSE Payment 2020US$/MWh`))
      lse_payment_plot_w_policy_neg <- lse_payment_plot_w_policy %>%
        filter(`LSE Payment 2020US$/MWh` <0)%>%
        group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
        mutate(pos = rev(cumsum(rev(`LSE Payment 2020US$/MWh`))) - (0.5 * `LSE Payment 2020US$/MWh`))
      lse_payment_plot_w_policy = rbind(lse_payment_plot_w_policy_pos, lse_payment_plot_w_policy_neg)
      
      lse_payment_plot_w_policy$cost_label = formatC(round(lse_payment_plot_w_policy$cost_label,1),format = 'f', digits = 1)
      lse_payment_plot_w_policy$cost_label[which(abs(as.numeric(lse_payment_plot_w_policy$cost_label))<0.99)] <- ''

      
      positive = as_tibble_col(vapply(lse_payment_plot_w_policy$value, function(x) max(x,0), numeric(1)))
      colnames(positive) = 'positive'
      lse_payment_plot_w_policy_total <- cbind(lse_payment_plot_w_policy,positive) %>%
        group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`, Policy) %>%
        summarize(value = sum(value),
                  pos = sum(positive)) %>%
        mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1),
               pos = round(pos/`Gross Total`,1))
      
      for (w in c('Clean Energy Standard','Carbon Cap-and-Trade')){
        temp_lse_payment_plot_w_policy <- filter(lse_payment_plot_w_policy,
                                                 Policy == w)
        if (w == 'Clean Energy Standard') {
          xlabel = '% of Total Load Supported by Clean Energy '
          temp_load_cost_color = select(load_cost_color,-c(`RPS Total Payment`))
          temp_lse_payment_plot_w_policy$`Cost Type`[which(temp_lse_payment_plot_w_policy$`Cost Type` == 'RPS Total Payment')] <- 'RPS/CES Total Payment'
        }
        if (w == 'Carbon Cap-and-Trade') {
          xlabel = '% Emission Reduction Compared to the 2005 Level'
          temp_load_cost_color = select(load_cost_color,-c(`RPS/CES Total Payment`))
        }
        ggplot()+
          geom_col(data = temp_lse_payment_plot_w_policy,
                   aes(x = Scenario, 
                       y = `LSE Payment 2020US$/MWh`, 
                       fill = `Cost Type`), 
                   color='black',
                   size=0.1,width = 0.7) +
          geom_text(data = temp_lse_payment_plot_w_policy,
                    aes(x= Scenario, 
                        y = pos, 
                        label = cost_label), 
                    size = 3)+
          scale_fill_manual(values = temp_load_cost_color,name = NULL) +
          geom_text(data=filter(lse_payment_plot_w_policy_total, Policy == w), 
                    aes(x= Scenario,
                        y = pos,
                        label = paste0('Net = ', formatC(round(`LSE Payment 2020US$/MWh`,1),format="f", digits = 1))),
                    nudge_y = 2,
                    fontface = "italic",
                    size = 3)+
          geom_point(data=filter(lse_payment_plot_w_policy_total,
                                 Policy == w),
                     aes(x= Scenario,
                         y = `LSE Payment 2020US$/MWh`)) +
          geom_hline(yintercept = 0)+
          scale_y_continuous(limits = ylims,breaks = ybreaks)+
          theme_classic2() +
          xlab(xlabel)+
          theme(legend.position = c(0.45,0.9))+
          labs(caption = MajorTechSensitivity[k])+
          guides(fill = guide_legend(nrow = 3))+
          ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECost/LSECostBreakDown_capvsces_',w,'_',k,'.png'),
                 width = 7,
                 height = p_height)
      }
    }
    
    lse_payment_plot_w_policy_2019 = cbind(lse_payment_plot, Policy) %>%
      filter(year == '2019',
             TechSensitivity == 'Mid',
             Scenario %in% c('Cap-and-Trade (80% Reduction Compare to 2005 Level)')) %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity),
             Scenario = '2019 Reference',
             Policy = '2019 Reference')%>% 
      mutate(cost_label = `LSE Payment 2020US$/MWh`) %>%
      filter(`Cost Type` != 'NJ DG Cost')
    lse_payment_plot_w_policy_pos_2019 <- lse_payment_plot_w_policy_2019 %>%
      filter(`LSE Payment 2020US$/MWh` >=0)%>%
      group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
      mutate(pos = rev(cumsum(rev(`LSE Payment 2020US$/MWh`))) - (0.5 * `LSE Payment 2020US$/MWh`))
    lse_payment_plot_w_policy_neg_2019 <- lse_payment_plot_w_policy_2019 %>%
      filter(`LSE Payment 2020US$/MWh` <0)%>%
      group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
      mutate(pos = rev(cumsum(rev(`LSE Payment 2020US$/MWh`))) - (0.5 * `LSE Payment 2020US$/MWh`))
    lse_payment_plot_w_policy_2019 = rbind(lse_payment_plot_w_policy_pos_2019, lse_payment_plot_w_policy_neg_2019)
    
    lse_payment_plot_w_policy_2019$cost_label = formatC(round(lse_payment_plot_w_policy_2019$cost_label,1),format = 'f', digits = 1)
    lse_payment_plot_w_policy_2019$cost_label[which(abs(as.numeric(lse_payment_plot_w_policy_2019$cost_label))<0.99)] <- ''
    
    
    positive = as_tibble_col(vapply(lse_payment_plot_w_policy_2019$value, function(x) max(x,0), numeric(1)))
    colnames(positive) = 'positive'
    lse_payment_plot_w_policy_2019_total <- cbind(lse_payment_plot_w_policy_2019,positive) %>%
      group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`, Policy) %>%
      summarize(value = sum(value),
                pos = sum(positive)) %>%
      mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1),
             pos = round(pos/`Gross Total`,1))
    ggplot()+
      geom_col(data = lse_payment_plot_w_policy_2019,
               aes(x = Scenario, 
                   y = `LSE Payment 2020US$/MWh`, 
                   fill = `Cost Type`), 
               color='black',
               size=0.1,width = 0.7) +
      geom_text(data = lse_payment_plot_w_policy_2019,
                aes(x= Scenario, 
                    y = pos, 
                    label = cost_label), 
                size = 3)+
      scale_fill_manual(values = load_cost_color) +
      geom_text(data=lse_payment_plot_w_policy_2019_total, 
                aes(x= Scenario,
                    y = pos,
                    label = paste0('Net = ', formatC(round(`LSE Payment 2020US$/MWh`,1),format="f", digits = 1))),
                nudge_y = 2,
                fontface = "italic",
                size = 3)+
      geom_point(data=lse_payment_plot_w_policy_2019_total,
                 aes(x= Scenario,
                     y = `LSE Payment 2020US$/MWh`)) +
      geom_hline(yintercept = 0)+
      # facet_grid(.~Policy, scale = 'free_x') +  
      scale_y_continuous(limits = ylims, breaks = ybreaks)+
      # coord_cartesian(ylim = c(-20, 80))+
      xlab('')+
      theme_classic2()+
      theme(legend.position = "none")+
      labs(caption = 'Mid')+
      guides(fill = guide_legend(nrow = 3, title.position = "left"))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECost/LSECostBreakDown_capvsces_2019.png'),
             width = 2,
             height = p_height)
    
  }
  for (k in 1:length(MajorTechSensitivity)) {
    lse_payment_plot_w_policy_ref = cbind(lse_payment_plot, Policy) %>%
      filter(year == '2030',
             TechSensitivity == MajorTechSensitivity[k],
             Scenario == 'DD (No Policy Reference)') %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity),
             Scenario = 'Current Policy Reference',
             Policy = 'Current Policy Reference') %>% 
      mutate(cost_label = `LSE Payment 2020US$/MWh`) %>%
      filter(`Cost Type` != 'NJ DG Cost')
    lse_payment_plot_w_policy_pos_ref <- lse_payment_plot_w_policy_ref %>%
      filter(`LSE Payment 2020US$/MWh` >=0)%>%
      group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
      mutate(pos = rev(cumsum(rev(`LSE Payment 2020US$/MWh`))) - (0.5 * `LSE Payment 2020US$/MWh`))
    lse_payment_plot_w_policy_neg_ref <- lse_payment_plot_w_policy_ref %>%
      filter(`LSE Payment 2020US$/MWh` <0)%>%
      group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
      mutate(pos = rev(cumsum(rev(`LSE Payment 2020US$/MWh`))) - (0.5 * `LSE Payment 2020US$/MWh`))
    lse_payment_plot_w_policy_ref = rbind(lse_payment_plot_w_policy_pos_ref, lse_payment_plot_w_policy_neg_ref)
    lse_payment_plot_w_policy_ref$cost_label = formatC(round(lse_payment_plot_w_policy_ref$cost_label,1),format = 'f', digits = 1)
    lse_payment_plot_w_policy_ref$cost_label[which(abs(as.numeric(lse_payment_plot_w_policy_ref$cost_label))<0.99)] <- ''
    
    positive = as_tibble_col(vapply(lse_payment_plot_w_policy_ref$value, function(x) max(x,0), numeric(1)))
    colnames(positive) = 'positive'
    lse_payment_plot_w_policy_ref_total <- cbind(lse_payment_plot_w_policy_ref,positive) %>%
      group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`, Policy) %>%
      summarize(value = sum(value),
                pos = sum(positive)) %>%
      mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1),
             pos = round(pos/`Gross Total`,1))
    # lse_payment_plot_total_2019 <- lse_payment_plot_w_policy_2019[lse_payment_plot_w_policy_2019$`Cost Type`!='NJ DG Cost',] %>%
    #   group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`,Policy) %>%
    #   summarize(value = sum(value)) %>%
    #   mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1))   
    ggplot()+
      geom_col(data = lse_payment_plot_w_policy_ref,
               aes(x = Scenario, 
                   y = `LSE Payment 2020US$/MWh`, 
                   fill = `Cost Type`), 
               color='black',
               size=0.1,width = 0.7) +
      geom_text(data = lse_payment_plot_w_policy_ref,
                aes(x= Scenario, 
                    y = pos, 
                    label = cost_label), 
                size = 3)+
      scale_fill_manual(values = load_cost_color) +
      geom_text(data=lse_payment_plot_w_policy_ref_total, 
                aes(x= Scenario,
                    y = pos,
                    label = paste0('Net = ', formatC(round(`LSE Payment 2020US$/MWh`,1),format="f", digits = 1))),
                nudge_y = 2,
                fontface = "italic",
                size = 3)+
      geom_point(data=lse_payment_plot_w_policy_ref_total,
                 aes(x= Scenario,
                     y = `LSE Payment 2020US$/MWh`)) +
      geom_hline(yintercept = 0)+
      scale_y_continuous(limits = ylims, breaks = ybreaks)+
      xlab('')+
      theme_classic2()+
      theme(legend.position = "none")+
      labs(caption = MajorTechSensitivity[k])+
      guides(fill = guide_legend(nrow = 3, title.position = "left"))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECost/LSECostBreakDown_2030_',k,'.png'),
             width = 2,
             height = p_height)
  }
}
