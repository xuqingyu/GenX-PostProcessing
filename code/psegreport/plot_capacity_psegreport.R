source('./code/Header.R')
p_width = 9
p_height = 7
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
    gen_capacity_subregion_fn <- paste0(RunFdr,'/CompiledResults/',
                                        Subregions[i],'/Generation/Gen_Capacity_w_2019',
                                        temp_total_title,".csv")
  } else {
    gen_capacity_subregion_fn <- paste0(RunFdr,'/CompiledResults/',
                                        Subregions[i],'/Generation/Gen_Capacity_',
                                        temp_total_title,".csv")
  }
  scenario_list = c('Current Policy (CP)',
                    'Stated Policy (SP)', 'SP + High Solar', 'SP + 80% Instate', 'SP + High Solar + 80% Instate', 
                    'SP + Nuclear', 'SP + High Solar + Nuclear', 'SP + Nuclear + 80% Instate', 'SP + High Solar + Nuclear + 80% Instate', 
                    'Deep Decarbonization (DD)', 'DD + High Solar', 'DD + 80% Instate', 'DD + High Solar + 80% Instate', 
                    'DD + Nuclear',  'DD + High Solar + Nuclear',  'DD + Nuclear + 80% Instate', 'DD + High Solar + Nuclear + 80% Instate')
  if (file.exists(gen_capacity_subregion_fn)){
    gen_capacity_subregion_plot_all <- read_csv(gen_capacity_subregion_fn, col_types = cols()) %>%
      pivot_longer(!c(case,year,Fuel,Scenario, TechSensitivity),names_to = 'Capacity Type') %>%
      filter(`Capacity Type` == 'Capacity') %>%
      mutate(Scenario = factor(Scenario, levels = scenario_list),
             TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
             Fuel = factor(Fuel, levels = capacity_resource_levels),
             `Capacity (GW)` = value/1000,
             year = factor(year, levels = c(2019,2020,years))) %>%
      arrange(Fuel) %>%
      select(-c(value,`Capacity Type`))
    # limits <- gen_capacity_subregion_plot_all %>%
    #   group_by(Scenario,TechSensitivity,year) %>%
    #   summarize(maxcapacity = sum(`Capacity (GW)`))
    # limits <- max(limits$maxcapacity)
    if (temp_total_title == 'PJM') {
      limits = c(0,800)
      outputbreak = seq(0,800,100)
    }
    if (temp_total_title == 'New Jersey') {
      limits = c(0,60)
      outputbreak = seq(0,100,10)
    }    
    
    for (j in 1:n_comparison){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      if(length(temp_compared_scenario) >4) {plot_row = 2} else {plot_row = 1}
      for (k in 1:length(interested_sensitivity)) {
        gen_capacity_subregion_plot <- gen_capacity_subregion_plot_all %>%
          filter(Scenario %in% temp_compared_scenario,
                 TechSensitivity == interested_sensitivity[k]) %>%
          group_by(case,year,Scenario,TechSensitivity)%>%
          mutate(capacitylabel = round(`Capacity (GW)`,1))%>%
          mutate(pos = rev(cumsum(rev(capacitylabel))) - (0.5 * capacitylabel))
        gen_capacity_subregion_plot$capacitylabel[which(gen_capacity_subregion_plot$capacitylabel<0.49)] <-''
        ggplot()+
          geom_col(data = filter(gen_capacity_subregion_plot),
                   aes(x = as.character(year), y = `Capacity (GW)`, 
                       fill=factor(Fuel, levels = capacity_resource_levels)),
                   colour="black", size= 0.1 ) +
          geom_text(data = gen_capacity_subregion_plot,
                    aes(x=as.character(year), y = pos, label = formatC(capacitylabel,format="f", digits = 1)), 
                    size = 4)+
          scale_fill_manual(name = "Resources", values = fuel_colors) + 
          scale_y_continuous(breaks = outputbreak)+
          coord_cartesian(ylim=limits)+
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
          guides(fill = guide_legend(nrow = 2, title.position = "top"))+
          ggtitle(label = paste0('Generation Capacity of ', temp_total_title, ' under \nSensitivity ',interested_sensitivity[k]))+
          ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Graphics/GenCapacity/GenCapacity_',comparison[j],'_',temp_total_title,'_',k,'.eps'),
                 width = p_width,
                 height = p_height,
                 device = 'eps')
      }
    }
  }
}
