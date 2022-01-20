# Plotting
p_width = 10
p_height = 7
source('./code/Header.R')
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_payment_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_long_',temp_total_title,"_with2019_and_DG.csv")
  scenario_list = c('Current Policy (CP)',
                    'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                    'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                    'Deep Decarbonization (DD)', 'DD + High Solar', 'DD + 80% Instate', 'DD + High Solar + 80% Instate', 
                    'DD + Nuclear',  'DD + High Solar + Nuclear',  'DD + Nuclear + 80% Instate', 'DD + High Solar + Nuclear + 80% Instate')

  if (file.exists(lse_payment_subregion_fn)){
    lse_payment_plot <- read_csv(lse_payment_subregion_fn) %>%
      mutate(Scenario = factor(Scenario, levels = scenario_list),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             `Cost Type` =factor(`Cost Type`, levels = load_cost_type)) %>%
      arrange(`Cost Type`)
    case_temp <- unique(select(lse_payment_plot,case))
    n_case_temp <- dim(case_temp)[1]
    if (temp_total_title == 'PJM') {
      plotlimits = c(-10,80)
      outputbreak = seq(-10,80,10)
    }
    if (temp_total_title == 'New Jersey') {
      plotlimits = c(-10,100)
      outputbreak = seq(-10,100,10)
    }    
    for (j in 1:n_comparison){
      for (k in 1:length(interested_sensitivity)){
        temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
        if(length(temp_compared_scenario) >4) {plot_row = 2} else {plot_row = 1}
        lse_payment_plot_comparison <- lse_payment_plot %>%
          mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1)) %>%
          filter(Scenario %in% temp_compared_scenario, 
                 TechSensitivity == interested_sensitivity[k]) %>%
          mutate(cost_label = `LSE Payment 2020US$/MWh`); 
        
        lse_payment_plot_comparison_pos <- lse_payment_plot_comparison %>%
          filter(`LSE Payment 2020US$/MWh` >=0)%>%
          group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
          mutate(pos = rev(cumsum(rev(`LSE Payment 2020US$/MWh`))) - (0.5 * `LSE Payment 2020US$/MWh`))
        lse_payment_plot_comparison_neg <- lse_payment_plot_comparison %>%
          filter(`LSE Payment 2020US$/MWh` <0)%>%
          group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
          mutate(pos = rev(cumsum(rev(`LSE Payment 2020US$/MWh`))) - (0.5 * `LSE Payment 2020US$/MWh`))
        lse_payment_plot_comparison = rbind(lse_payment_plot_comparison_pos, lse_payment_plot_comparison_neg)
        lse_payment_plot_comparison$cost_label[which(abs(lse_payment_plot_comparison$cost_label)<0.5)] <- ''
          
        positive = as_tibble_col(vapply(lse_payment_plot_comparison$value, function(x) max(x,0), numeric(1)))
        colnames(positive) = 'positive'
        lse_payment_plot_total_withdg <- cbind(lse_payment_plot_comparison,positive) %>%
          group_by(case,year,AnnualLoad, Scenario, TechSensitivity,`Gross Total`) %>%
          summarize(value = sum(value),
                    pos = sum(positive)) %>%
          mutate(`LSE Payment 2020US$/MWh` = round(value/`Gross Total`,1),
                 pos = round(pos/`Gross Total`,1))
        
        ggplot()+
          geom_col(data = lse_payment_plot_comparison,
                   aes(x=as.character(year), y = `LSE Payment 2020US$/MWh`, fill = `Cost Type`),color='black',size=0.1,) +
          geom_text(data = lse_payment_plot_comparison,
                    aes(x=as.character(year), y = pos, label = formatC(cost_label,format="f", digits = 1)), size = 3)+
          scale_fill_manual(values = load_cost_color) +
          # geom_line(data=lse_payment_plot_total_withdg, aes(x=as.character(year),y = `LSE Payment 2020US$/MWh`)) + 
          # geom_point(data=lse_payment_plot_total_withdg, aes(x=as.character(year),y = `LSE Payment 2020US$/MWh`)) +
          geom_text(data=lse_payment_plot_total_withdg, aes(x=as.character(year),y = pos,
                                                            label = paste0('Net = ', formatC(`LSE Payment 2020US$/MWh`,format="f", digits = 1))),
                    nudge_y = 3,
                    fontface = "italic",
                    size = 3)+
          scale_y_continuous(breaks = outputbreak)+
          coord_cartesian(ylim = plotlimits)+
          geom_hline(yintercept = 0)+
          facet_grid(.~Scenario) + 
          xlab('Year')+
          theme_bw()+
          theme(legend.position = "bottom")+
          facet_wrap(.~Scenario, nrow = plot_row) +
          guides(fill = guide_legend(nrow = 2, title.position = "left"))+
          ggtitle(label = paste0('LSE Cost of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
          ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECost/LSECostBreakDown_',comparison[j],'_',
                        temp_total_title,'_',k,'_with_NJ_DG.eps'),
                 width = p_width,
                 height= p_height,
                 device = 'eps')
      }
    }
  }
}

