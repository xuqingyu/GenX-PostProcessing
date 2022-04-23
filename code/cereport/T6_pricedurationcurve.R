# Duration curve
energyprice_ts <- read_csv(paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries.csv')) %>%
  left_join(cases_newnames,by=c('case' = 'case_description')) %>%
  mutate(Scenario = factor(Scenario, levels = c(scenario,'Current Policy Reference')),
         TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
         year = factor(year, levels = years))
temp_compared_scenario = c('Cap-and-Trade (70% Reduction Compare to 2005 Level)',
                           'Cap-and-Trade (75% Reduction Compare to 2005 Level)',
                           'Cap-and-Trade (80% Reduction Compare to 2005 Level)',
                           'Cap-and-Trade (85% Reduction Compare to 2005 Level)',
                           'DD (No Policy Reference)')
for (k in 1:length(interested_sensitivity)){
  temp_plot <- energyprice_ts  %>%
    filter(Scenario %in% temp_compared_scenario,
           Region == 'PJM_WEST',
           TechSensitivity == interested_sensitivity[k],
           year == 2030) %>%
    arrange(Scenario, Region, Zone, case, year, desc(Price))
  temp_plot$Scenario[which(temp_plot$Scenario == 'DD (No Policy Reference)')] <- 'Current Policy Reference'
  rank = rep(c(1:8736),length.out = nrow(temp_plot))
  temp_plot = cbind(temp_plot,rank)
  
  ggplot(data=temp_plot,aes(x=rank, y=Price, color = Scenario)) +
    scale_color_brewer(palette = 'Set1') +
    scale_y_continuous(limits = c(-100,300),breaks = seq(-100,400,50))+
    geom_line()+
    theme_classic2()+
    geom_hline(yintercept = 0, color = 'grey30')+
    theme(legend.position = c(0.65,0.7),
          # legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    labs(x = 'Hour Rank', y = "Energy Price ($/MWh)")+
    guides(color = guide_legend(ncol = 1))+
    # ggtitle(paste0('Energy Price Duration curves Under \nSensitivity ',interested_sensitivity[k]))+
    # ylab("Energy Price ($/MWh)") + 
    ggsave(paste0(RunFdr,'/Graphics/EnergyPrice/Cat_Duration Curve Energy Price of Region_',k,'.png'),
           width = 7,
           height= 7)
  ggplot(data=temp_plot,aes(x=Price,after_stat(density), color = Scenario)) +
    scale_color_brewer(palette = 'Set1') +
    scale_x_continuous(limits = c(-100,300),breaks = seq(-100,400,50))+
    scale_y_continuous(limits = c(0,.5),breaks = seq(0,.5,0.05))+
    geom_freqpoly(binwidth = 1)+
    theme_classic2()+
    geom_hline(yintercept = 0, color = 'grey30')+
    theme(legend.position = c(0.65,0.7),
          # legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1))+
    # ggtitle(paste0('Energy Price Distribution curves Under \nSensitivity ',interested_sensitivity[k]))+
    ylab("Density") + 
    xlab("Energy Price ($/MWh)") +
    ggsave(paste0(RunFdr,'/Graphics/EnergyPrice/Cat_Density Energy Price of Region_',k,'.png'),
           width = 7,
           height= 7)
}

temp_compared_scenario = c('Clean Energy Standard (70%)',
                           'Clean Energy Standard (75%)',
                           'Clean Energy Standard (80%)',
                           'Clean Energy Standard (85%)',
                           'DD (No Policy Reference)')
for (k in 1:length(interested_sensitivity)){
  temp_plot <- energyprice_ts  %>%
    filter(Scenario %in% temp_compared_scenario,
           Region == 'PJM_WEST',
           TechSensitivity == interested_sensitivity[k],
           year == 2030) %>%
    arrange(Scenario, Region, Zone, case, year, desc(Price))
  temp_plot$Scenario[which(temp_plot$Scenario == 'DD (No Policy Reference)')] <- 'Current Policy Reference'
  rank = rep(c(1:8736),length.out = nrow(temp_plot))
  temp_plot = cbind(temp_plot,rank)
  
  ggplot(data=temp_plot,aes(x=rank, y=Price, color = Scenario)) +
    scale_color_brewer(palette = 'Set1') +
    scale_y_continuous(limits = c(-100,300),breaks = seq(-100,400,50))+
    geom_line()+
    theme_classic2()+
    geom_hline(yintercept = 0, color = 'grey30')+
    theme(legend.position = c(0.65,0.7),
          # legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1))+
    labs(x = 'Hour Rank', y = "Energy Price ($/MWh)")+
    # ggtitle(paste0('Energy Price Duration curves Under \nSensitivity ',interested_sensitivity[k]))+
    # ylab("Energy Price ($/MWh)") + 
    ggsave(paste0(RunFdr,'/Graphics/EnergyPrice/Ces_Duration Curve Energy Price of Region_',k,'.png'),
           width = 7,
           height= 7)
  ggplot(data=temp_plot,aes(x=Price,after_stat(density), color = Scenario)) +
    scale_color_brewer(palette = 'Set1') +
    scale_x_continuous(limits = c(-100,300),breaks = seq(-100,400,50))+
    scale_y_continuous(limits = c(0,.5),breaks = seq(0,.5,0.05))+
    geom_freqpoly(binwidth = 1)+
    theme_classic2()+
    geom_hline(yintercept = 0, color = 'grey30')+
    theme(legend.position = c(0.65,0.7),
          # legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1))+
    # ggtitle(paste0('Energy Price Distribution curves Under \nSensitivity ',interested_sensitivity[k]))+
    ylab("Density") + 
    xlab("Energy Price ($/MWh)") +
    ggsave(paste0(RunFdr,'/Graphics/EnergyPrice/Ces_Density Energy Price of Region_',k,'.png'),
           width = 7,
           height= 7)
}

