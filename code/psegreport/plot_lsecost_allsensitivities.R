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
    lse_payment_plot_total <- read_csv(lse_payment_subregion_fn) %>%
      mutate(Scenario = factor(Scenario, levels = rev(scenario_list)),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             `Cost Type` =factor(`Cost Type`, levels = load_cost_type)) %>%
      arrange(`Cost Type`) %>%
      group_by(Scenario, TechSensitivity,year) %>%
      summarize(`LSE Payment 2020US$/MWh` = sum(round(value/`Gross Total`,1))) %>%
      filter(Scenario %in% c('Current Policy (CP)',
                             'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                             'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                             'Deep Decarbonization (DD)')) %>%
      filter(TechSensitivity %in% c('Mid', 'Low RE/BESS Cost','Low NatGas Price','High RE/BESS Cost','High NatGas Price')) %>%
      ungroup()%>%
      group_by(Scenario,year) %>%
      mutate(minlsecost = min(`LSE Payment 2020US$/MWh`),maxlsecost = max(`LSE Payment 2020US$/MWh`))
    lse_payment_plot_total_2019 <- lse_payment_plot_total %>%
      filter(TechSensitivity == 'Mid', year == 2019)
    lse_payment_plot_total <- lse_payment_plot_total %>%
      filter(year != 2019)
    ggplot()+
      geom_hline(yintercept = mean(lse_payment_plot_total_2019$`LSE Payment 2020US$/MWh`), color = 'gray70' )+
      geom_linerange(data = lse_payment_plot_total,
                     aes(x = Scenario, ymin = minlsecost, ymax = maxlsecost),
                     size = 0.5) +
      geom_point(data = filter(lse_payment_plot_total, TechSensitivity != 'Mid'),
                 aes(x = Scenario, y = `LSE Payment 2020US$/MWh`, color = TechSensitivity), 
                 shape = 16, size = 3)+
      geom_point(data = filter(lse_payment_plot_total, TechSensitivity == 'Mid'),
                 aes(x = Scenario, y = `LSE Payment 2020US$/MWh`, color = TechSensitivity), 
                 shape = 16, size = 4)+
      coord_flip() +
      scale_color_brewer(palette = 'Set1', name = 'Sensitivities') +
      theme_bw() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
      ) + 
      facet_wrap(.~year, nrow = 1) + 
      guides(color = guide_legend(nrow = 1, title.position = "top")) +
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/LSECost/LSEcostallcase_',temp_total_title,'_.eps'),
             width = p_width,
             height = p_height,
             device = 'eps')
  }
}


