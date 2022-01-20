p_width = 10
p_height = 4
RunFdr <- "/Users/qingyuxu/Dropbox (Princeton)/Old Results/PJM_QX_2030_ALL_18x7_newwacc/"
settingfile <- 'sample_inputs_pjm.csv';
source('./code/Header.R')
for (i in 2:2) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  capacityvalue_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Capacity_Values_',temp_total_title,".csv")
  scenario_list = c('Current Policy (CP)',
                    'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                    'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                    'Deep Decarbonization (DD)', 'DD + High Solar', 'DD + 80% Instate', 'DD + High Solar + 80% Instate', 
                    'DD + Nuclear',  'DD + High Solar + Nuclear',  'DD + Nuclear + 80% Instate', 'DD + High Solar + Nuclear + 80% Instate')
  
  if (file.exists(capacityvalue_subregion_fn)){
    capacityvalue_subregion <- read_csv(capacityvalue_subregion_fn) %>%
      mutate(Scenario = factor(Scenario, levels = rev(scenario_list)),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity)) %>%
      filter(Scenario %in% c('Current Policy (CP)',
                             'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                             'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                             'Deep Decarbonization (DD)')) %>%
      filter(TechSensitivity %in% c('Mid', 'Low RE/BESS Cost','Low NatGas Price','High RE/BESS Cost','High NatGas Price')) %>%
      group_by(Scenario, year,Fuel,Reserve) %>%
      mutate(minvalue = min(`Capacity Value`),maxvalue = max(`Capacity Value`))
    for(w in c('Offshore Wind', 'Utility Solar','Battery')) {
      for (k in c('CapRes_1','CapRes_2')) {
        ggplot()+
          geom_linerange(data = filter(capacityvalue_subregion,Fuel == w, Reserve == k),
                         aes(x = Scenario, ymin = minvalue, ymax = maxvalue),
                         size = 0.5) +
          geom_point(data = filter(capacityvalue_subregion,Fuel == w, Reserve == k),
                     aes(x = Scenario, y = `Capacity Value`, color = TechSensitivity), 
                     shape = 16, size = 3)+
          coord_flip() +
          scale_color_brewer(palette = 'Set1', name = 'Sensitivities') +
          scale_y_continuous(limits = c(0,1),breaks = seq(0,1,.1))+
          ylab('CapacityValue')+
          theme_bw() +
          theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = 'bottom',
          ) + 
          facet_wrap(.~year, nrow = 1) + 
          guides(color = guide_legend(nrow = 1, title.position = "top")) +
          ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/Gen_CapacityValue_',temp_total_title,'_of_',w,'_',k,'_.eps'),
                 width = p_width,
                 height = p_height,
                 device = 'eps')
      }
    }
  }
}


