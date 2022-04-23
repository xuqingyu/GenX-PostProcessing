ScenarioFilter = c('Cap-and-Trade (40% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (45% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (100% Reduction Compare to 2005 Level)',
                   'Clean Energy Standard (40%)',
                   'Clean Energy Standard (45%)',
                   'Clean Energy Standard (100%)')
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
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  genprofit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Profit_',temp_total_title,'.csv')
  
  if (file.exists(genprofit_subregion_fn)){
    genprofit_subregion_plot <- read_csv(genprofit_subregion_fn, col_types = cols()) %>%
      select(-c(AnnualCharge,Capacity,`Energy Capacity`,`Charge Capacity`)) %>%
      rename(`RPS Revenue`=`ESR Revenue`) %>%
      filter(!(Fuel %in% storage_fuel),
             AnnualOutput >= 1000) %>%
      pivot_longer(cols = !c(case, year, Fuel, Scenario, TechSensitivity, AnnualOutput),names_to = 'Revenue/Cost Type') %>%
      mutate(TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             `Revenue/Cost Type` = factor(`Revenue/Cost Type`, levels = genprofit_type)) %>%
      filter(!(Scenario %in% ScenarioFilter))
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
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (80% Reduction Compare to 2005 Level)'] = '80%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (90% Reduction Compare to 2005 Level)'] = '90%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (75% Reduction Compare to 2005 Level)'] = '75%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (70% Reduction Compare to 2005 Level)'] = '70%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (60% Reduction Compare to 2005 Level)'] = '60%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (50% Reduction Compare to 2005 Level)'] = '50%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (55% Reduction Compare to 2005 Level)'] = '55%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (65% Reduction Compare to 2005 Level)'] = '65%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (85% Reduction Compare to 2005 Level)'] = '85%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Cap-and-Trade (95% Reduction Compare to 2005 Level)'] = '95%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (65%)'] = '65%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (70%)'] = '70%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (75%)'] = '75%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (80%)'] = '80%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (85%)'] = '85%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (50%)'] = '50%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (55%)'] = '55%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (60%)'] = '60%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (90%)'] = '90%'
    gen_profit_subregion_plot_w_policy$Scenario[gen_profit_subregion_plot_w_policy$Scenario == 'Clean Energy Standard (95%)'] = '95%'
    
    for (k in 1:length(MajorTechSensitivity)) {
      for (w in c('Clean Energy Standard','Carbon Cap-and-Trade')){

          genprofit_subregion_plot_comparison <- gen_profit_subregion_plot_w_policy %>%
            filter(TechSensitivity == MajorTechSensitivity[k],
                   Policy == w) %>%
            mutate(`Revenue/Cost 2020$/MWh` = round(value/AnnualOutput,2)) %>%
            mutate(cost_label = `Revenue/Cost 2020$/MWh`) %>%
            arrange(`Revenue/Cost Type`)
          if (w == 'Clean Energy Standard') {
            xlabel = '% of Total Load Supported by Clean Energy '
            temp_genprofit_color = select(genprofit_color,-c(`RPS Revenue`))
            genprofit_subregion_plot_comparison$`Revenue/Cost Type`[which(genprofit_subregion_plot_comparison$`Revenue/Cost Type` == 'RPS Revenue')] <- 'RPS/CES Total Revenue'
          }
          if (w == 'Carbon Cap-and-Trade') {
            xlabel = '% Emission Reduction Compared to the 2005 Level'
            temp_genprofit_color = select(genprofit_color,-c(`RPS/CES Total Revenue`))
          }          
          genprofit_subregion_plot_comparison_pos <- genprofit_subregion_plot_comparison %>%
            filter(`Revenue/Cost 2020$/MWh` >=0)%>%
            group_by(case, year, Fuel, Scenario, TechSensitivity, `AnnualOutput`) %>%
            mutate(pos = rev(cumsum(rev(`Revenue/Cost 2020$/MWh`))) - (0.5 * `Revenue/Cost 2020$/MWh`))
          genprofit_subregion_plot_comparison_neg <- genprofit_subregion_plot_comparison %>%
            filter(`Revenue/Cost 2020$/MWh` <0)%>%
            group_by(case, year, Fuel, Scenario, TechSensitivity, `AnnualOutput`) %>%
            mutate(pos = rev(cumsum(rev(`Revenue/Cost 2020$/MWh`))) - (0.5 * `Revenue/Cost 2020$/MWh`))
          genprofit_subregion_plot_comparison = rbind(genprofit_subregion_plot_comparison_pos, genprofit_subregion_plot_comparison_neg)
          
          genprofit_subregion_plot_comparison$cost_label = formatC(round(genprofit_subregion_plot_comparison$cost_label,1),format = 'f', digits = 1)
          genprofit_subregion_plot_comparison$cost_label[which(abs(as.numeric(genprofit_subregion_plot_comparison$cost_label))<5)] <- ''
          
          
          positive = as_tibble_col(vapply(genprofit_subregion_plot_comparison$value, function(x) max(x,0), numeric(1)))
          colnames(positive) = 'positive'
          
          genprofit_subregion_plot_total <- cbind(genprofit_subregion_plot_comparison,positive) %>%
            group_by(case, year, Fuel, Scenario, TechSensitivity, AnnualOutput, Policy) %>%
            summarize(value = sum(value),
                      pos = sum(positive)) %>%
            mutate(`Revenue/Cost 2020$/MWh` = round(value/AnnualOutput,2),
                   pos = round(pos/AnnualOutput,1))
        for (j in c('Gas CC', 'Nuclear','Utility Solar', 'Onshore Wind', 'Offshore Wind')){
          ggplot()+
            geom_col(data = filter(genprofit_subregion_plot_comparison, 
                                   Fuel == j),
                     aes(x = Scenario, 
                         y = `Revenue/Cost 2020$/MWh`, 
                         fill = `Revenue/Cost Type`),
                     color='black',
                     size=0.1) +
            geom_text(data = filter(genprofit_subregion_plot_comparison, 
                                    Fuel == j),
                      aes(x= Scenario, 
                          y = pos, 
                          label = cost_label), 
                      size = 3)+
            scale_fill_manual(values = temp_genprofit_color) +
            geom_point(data=filter(genprofit_subregion_plot_total,
                                   Fuel == j), 
                       aes(x = Scenario, 
                           y = `Revenue/Cost 2020$/MWh`)) +
            geom_text(data=filter(genprofit_subregion_plot_total, 
                                  Fuel == j), 
                      aes(x= Scenario,
                          y = pos,
                          label = paste0('Net = ', formatC(round(`Revenue/Cost 2020$/MWh`,1),format="f", digits = 1))),
                      nudge_y = 2,
                      fontface = "italic",
                      size = 3)+
            geom_hline(yintercept = 0)+
            coord_cartesian(ylim = c(-180,180))+
            scale_y_continuous(breaks = seq(-200,200,20))+
            # facet_grid(.~Policy) + 
            theme_classic2()+
            theme(legend.position = c(.45,.9))+
            labs(caption = paste0(j,' ', MajorTechSensitivity[k]))+
            xlab(xlabel)+
            guides(fill = guide_legend(nrow = 4, title.position = NULL))+
            # ggtitle(label = paste0('Generation Profit of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k],' of technology ', list_of_tech[w]))+
            ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenProfit/',j,'_ProfitBreakDown_',temp_total_title,'_',k,'_',w,'.png'),
                   width = 7,
                   height = p_height)
          
        }
      }
    }
  }
}
