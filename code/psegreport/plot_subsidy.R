scenario_list = c('Current Policy (CP)',
                  'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                  'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                  'Deep Decarbonization (DD)', 'DD + High Solar', 'DD + 80% Instate', 'DD + High Solar + 80% Instate', 
                  'DD + Nuclear',  'DD + High Solar + Nuclear',  'DD + Nuclear + 80% Instate', 'DD + High Solar + Nuclear + 80% Instate')

total_subsidy = read_csv('./data/totalsubsidy.csv') %>%
  mutate(Scenario = factor(Scenario, scenario_list))
p_width = 10
p_height = 7
for (j in 1:n_comparison) {
  temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
  if(length(temp_compared_scenario) >4) {plot_row = 2} else {plot_row = 1}
  for (k in 1:length(interested_sensitivity)) {
    total_subsidy_plot <- total_subsidy %>%
      filter(Scenario %in% temp_compared_scenario,
             TechSensitivity == interested_sensitivity[k]) %>%
      group_by(case,year,Scenario,TechSensitivity)
    ggplot()+
      geom_point(data = filter(total_subsidy_plot),
               aes(x = as.numeric(year), y = `Total Subsidy`, 
                   color=factor(Fuel, levels = capacity_resource_levels)),
               size= 2, shape = 4 ) +
      # scale_color_manual(name = "Resources", values = fuel_colors) + 
      geom_line(data = filter(total_subsidy_plot),
                 aes(x = as.numeric(year), y = `Total Subsidy`,
                     color=factor(Fuel, levels = capacity_resource_levels)),
                linetype = 1,size = 1) +
      # geom_text(data = filter(total_subsidy_plot),
      #           aes(x = as.numeric(year), y = `Total Subsidy`, label = `Total Subsidy`),
      #           size = 4) +
      scale_color_brewer(palette = 'Set1', name = 'Resource')+
      scale_y_continuous(breaks = seq(0,150,20))+
      scale_x_continuous(breaks = c(2030,2040,2050))+
      coord_cartesian(ylim=c(0,150))+
      geom_hline(yintercept=0) + 
      facet_wrap(.~Scenario, nrow = plot_row) + 
      theme_bw()+
      # coord_cartesian(ylim = c(0, limits))+
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom'
      )+
      xlab('Year')+
      ylab('Total Subsidy ($/MWh Gen.)')+
      guides(color = guide_legend(nrow = 1, title.position = "top"))+
      ggtitle(label = paste0('Total subsidy of each technology in New Jersey under \nSensitivity ',interested_sensitivity[k]))+
      ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenSubsidy_',comparison[j],'_','New Jersey','_',k,'.eps'),
             width = p_width,
             height = p_height,
             device = 'eps')
  }
}
 