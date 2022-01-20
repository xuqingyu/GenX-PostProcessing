p_width = 10
p_height = 4
source('./code/Header.R')
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  capturedenergyprice_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_CapturedEnergyPrice_',temp_total_title,".csv")
  scenario_list = c('Current Policy (CP)',
                    'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                    'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                    'Deep Decarbonization (DD)', 'DD + High Solar', 'DD + 80% Instate', 'DD + High Solar + 80% Instate', 
                    'DD + Nuclear',  'DD + High Solar + Nuclear',  'DD + Nuclear + 80% Instate', 'DD + High Solar + Nuclear + 80% Instate')
  
  if (file.exists(capturedenergyprice_subregion_fn)){
    capturedenergyprice_subregion <- read_csv(capturedenergyprice_subregion_fn) %>%
      mutate(Scenario = factor(Scenario, levels = rev(scenario_list)),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity)) %>%
      filter(Scenario %in% c('Current Policy (CP)',
                             'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                             'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                             'Deep Decarbonization (DD)')) %>%
      filter(TechSensitivity %in% c('Mid', 'Low RE/BESS Cost','Low NatGas Price','High RE/BESS Cost','High NatGas Price')) %>%
      group_by(Scenario, year,Fuel) %>%
      mutate(minprice = min(`Energy Revenue per MWh`),maxprice = max(`Energy Revenue per MWh`))
    for(w in c('Offshore Wind', 'Utility Solar')) {
      ggplot()+
        geom_linerange(data = filter(capturedenergyprice_subregion,Fuel == w),
                       aes(x = Scenario, ymin = minprice, ymax = maxprice),
                       size = 0.5) +
        geom_point(data = filter(capturedenergyprice_subregion,Fuel == w),
                   aes(x = Scenario, y = `Energy Revenue per MWh`, color = TechSensitivity), 
                   shape = 16, size = 3)+
        coord_flip() +
        scale_color_brewer(palette = 'Set1', name = 'Sensitivities') +
        scale_y_continuous(limits = c(0,60),breaks = seq(0,100,10))+
        ylab('Energy Revenue per MWh (2020$/MWh)')+
        theme_bw() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = 'bottom',
        ) + 
        facet_wrap(.~year, nrow = 1) + 
        guides(color = guide_legend(nrow = 1, title.position = "top")) +
        ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Gen_Captured_Price_',temp_total_title,'_of_',w,'_.eps'),
               width = p_width,
               height = p_height,
               device = 'eps')
    }
  }
}


