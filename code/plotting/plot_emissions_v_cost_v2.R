# Plot Policy comparison emissions vs system cost
# Created by: Qingyu Xu
# Created on: 07-21-2021
settingfile <- 'sample_inputs_pjm.csv';
RunFdr <- '/Users/qingyuxu/Documents/PJM_QX_2022_PH1_newwacc'
source('./code/Header.R')
MajorTechSensitivity <- c('Mid',
                          'Low RE/BESS Cost', 
                          'Low NatGas Price',
                          'High RE/BESS Cost',
                          'High NatGas Price',
                          'No Interregional Transmission Upgrade',
                          'Allow CCS Expansion')
# Reference_Emission <- as_tibble(cbind(`Load Emissions Rate (Ton/MWh)` = c(0.466, 0.304, 0.243, 0.182, 0.152, 0.122, 0.061),
#                                 Reference = c('2019 Level (simulated)','50% Emission Reduction of 2005 level',
#                                                     '60% Emission Reduction','70% Emission Reduction','75% Emission Reduction',
#                                                     '80% Emission Reduction','90% Emission Reduction')))
Reference_Emission <- as_tibble(cbind(`Load Emissions Rate (Ton/MWh)` = c(0.466, 0.304, 0.122),
                                      Reference = c('2019 Level (simulated)','50% of 2005 level',
                                                    '20% of 2005 level')))
Reference_Emission$`Load Emissions Rate (Ton/MWh)` = as.numeric(Reference_Emission$`Load Emissions Rate (Ton/MWh)`)

for (i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  system_cost_vs_emission <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                             Subregions[i],'/System_Cost_Emission_Tradeoff',
                                             temp_total_title,".csv"),
                                      col_types = cols()) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
  cost_max <- max(system_cost_vs_emission$`System Cost ($/MWh)`)
  cost_min <- min(system_cost_vs_emission$`System Cost ($/MWh)`)
  ler_max <- max(system_cost_vs_emission$`Load Emissions Rate (Ton/MWh)`[system_cost_vs_emission$year == '2030'])
  Policy = rep('No Federal Policy',nrow(system_cost_vs_emission))
  Policy[grep('Clean Energy Standard',system_cost_vs_emission$Scenario)] = 
    'Clean Energy Standard';
  Policy[grep('Cap-and-Trade',system_cost_vs_emission$Scenario)] = 
    'Carbon Cap-and-Trade';
  
  system_cost_vs_emission_w_policy = cbind(system_cost_vs_emission, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy')
  cost_max <- max(system_cost_vs_emission_w_policy$`System Cost ($/MWh)`)
  cost_min <- min(system_cost_vs_emission_w_policy$`System Cost ($/MWh)`)
  ggplot() +
    geom_point(data = system_cost_vs_emission_w_policy, 
               aes(x = `System Cost ($/MWh)`, 
                   y = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) + 
    geom_path(data = system_cost_vs_emission_w_policy, 
              aes(x = `System Cost ($/MWh)`, 
                  y = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity)) + 
    facet_grid(.~Policy)+
    geom_hline(data = Reference_Emission, aes(yintercept = `Load Emissions Rate (Ton/MWh)`, linetype = Reference)) +
    scale_color_brewer(palette = 'Set2') +
    coord_cartesian(xlim = c(cost_min, cost_max), ylim = c(0, ler_max))+
    theme_bw() +
    theme(legend.position = 'right')+
    guides(color = guide_legend(ncol = 1, title.position = "top"),
           linetype = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/system_emission_tradeoff_v2.png'),width = 12,height=6)

  system_cost_vs_emission_w_policy_recost <- cbind(system_cost_vs_emission, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% c('Mid', 'Low RE/BESS Cost','High RE/BESS Cost'),
           Policy != 'No Federal Policy')
  # ggplot() +
  #   geom_point(data = system_cost_vs_emission_w_policy_recost, 
  #              aes(y = `System Cost ($/MWh)`, 
  #                  x = `Load Emissions Rate (Ton/MWh)`,
  #                  color = TechSensitivity)) + 
  #   geom_path(data = system_cost_vs_emission_w_policy_recost, 
  #             aes(y = `System Cost ($/MWh)`, 
  #                 x = `Load Emissions Rate (Ton/MWh)`,
  #                 color = TechSensitivity)) + 
  #   facet_grid(.~Policy)+
  #   geom_vline(data = Reference_Emission, 
  #              aes(xintercept = `Load Emissions Rate (Ton/MWh)`, 
  #                  linetype = Reference),
  #              show.legend = F) +
  #   geom_vline(xintercept = 0, color = 'red') +
  #   scale_color_brewer(palette = 'Set2') +
  #   coord_cartesian(ylim = c(cost_min, cost_max))+
  #   scale_x_reverse(limits = c(ler_max,0))+
  #   theme_bw(base_line_size = 0.25) +
  #   theme(legend.position = 'right',legend.key = element_rect(fill = "white", colour = "black"))+
  #   guides(color = guide_legend(ncol = 1, title.position = "top"),
  #          linetype = guide_legend(ncol = 1, title.position = "top"))+
  #   ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
  #                 '/Graphics/system_emission_tradeoff_reverse_recost_v2.png'),width = 12,height=6)
  # 
  # system_cost_vs_emission_w_policy_ngcost <- cbind(system_cost_vs_emission, Policy) %>%
  #   filter(year == '2030',
  #          TechSensitivity %in% c('Mid', 'Low NatGas Price','High NatGas Price'),
  #          Policy != 'No Federal Policy')
  # ggplot() +
  #   geom_point(data = system_cost_vs_emission_w_policy_ngcost, 
  #              aes(y = `System Cost ($/MWh)`, 
  #                  x = `Load Emissions Rate (Ton/MWh)`,
  #                  color = TechSensitivity)) + 
  #   geom_path(data = system_cost_vs_emission_w_policy_ngcost, 
  #             aes(y = `System Cost ($/MWh)`, 
  #                 x = `Load Emissions Rate (Ton/MWh)`,
  #                 color = TechSensitivity)) + 
  #   facet_grid(.~Policy)+
  #   geom_vline(data = Reference_Emission, 
  #              aes(xintercept = `Load Emissions Rate (Ton/MWh)`, 
  #                  linetype = Reference),
  #              show.legend = F) +
  #   geom_vline(xintercept = 0, color = 'red') +
  #   scale_color_brewer(palette = 'Set2') +
  #   coord_cartesian(ylim = c(cost_min, cost_max))+
  #   scale_x_reverse(limits = c(ler_max,0))+
  #   theme_bw(base_line_size = 0.25) +
  #   theme(legend.position = 'right',legend.key = element_rect(fill = "white", colour = "black"))+
  #   guides(color = guide_legend(ncol = 1, title.position = "top"),
  #          linetype = guide_legend(ncol = 1, title.position = "top"))+
  #   ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
  #                 '/Graphics/system_emission_tradeoff_reverse_ngcost_v2.png'),width = 12,height=6)
  
  MajorTechSensitivity <- c('Mid',
                            'Low RE/BESS Cost', 
                            'Low NatGas Price',
                            'High RE/BESS Cost',
                            'High NatGas Price')    
  ggplot() +
    geom_point(data = system_cost_vs_emission_w_policy, 
               aes(y = `System Cost ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) + 
    geom_path(data = system_cost_vs_emission_w_policy, 
              aes(y = `System Cost ($/MWh)`, 
                  x = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity)) + 
    facet_grid(.~Policy)+
    geom_vline(data = Reference_Emission, aes(xintercept = `Load Emissions Rate (Ton/MWh)`, linetype = Reference)) +
    geom_vline(xintercept = 0, color = 'red') +
    scale_color_brewer(palette = 'Set2') +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_x_reverse(limits = c(ler_max,0))+
    theme_bw(base_line_size = 0.25) +
    theme(legend.position = 'right',legend.key = element_rect(fill = "white", colour = "black"))+
    guides(color = guide_legend(ncol = 1, title.position = "top"),
           linetype = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/system_emission_tradeoff_reverse_v2.png'),width = 12,height=6)

  ces_system_cost_vs_emission = filter(system_cost_vs_emission,
                                       year == '2030',
                                       grepl('Clean Energy Standard', Scenario),
                                       TechSensitivity %in% MajorTechSensitivity)  
  ggplot() +
    geom_point(data = ces_system_cost_vs_emission, 
               aes(x = `System Cost ($/MWh)`, 
                   y = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) + 
    geom_path(data = ces_system_cost_vs_emission, 
              aes(x = `System Cost ($/MWh)`, 
                  y = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity)) + 
    geom_hline(data = Reference_Emission, aes(yintercept = `Load Emissions Rate (Ton/MWh)`, linetype = Reference)) +
    scale_color_brewer(palette = 'Set2') +
    coord_cartesian(xlim = c(cost_min, cost_max), ylim = c(0, ler_max))+
    theme_classic() +
    theme(legend.position = 'right')+
    guides(color = guide_legend(ncol = 1, title.position = "top"),
           linetype = guide_legend(ncol = 1, title.position = "top"))+
    ggtitle('Clean Energy Standard') +
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/system_emission_tradeoff_ces_v2.png'),width = 9,height=7)
  cap_system_cost_vs_emission = filter(system_cost_vs_emission,
                                       year == '2030',
                                       grepl('Cap-and-Trade', Scenario),
                                       TechSensitivity %in% MajorTechSensitivity)
  ggplot() +
    geom_point(data = cap_system_cost_vs_emission, 
               aes(x = `System Cost ($/MWh)`, 
                   y = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) + 
    geom_path(data = cap_system_cost_vs_emission, 
              aes(x = `System Cost ($/MWh)`, 
                  y = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity)) + 
    geom_hline(data = Reference_Emission, aes(yintercept = `Load Emissions Rate (Ton/MWh)`, linetype = Reference)) +
    scale_color_brewer(palette = 'Set2') +
    coord_cartesian(xlim = c(cost_min, cost_max), ylim = c(0, ler_max))+
    theme_classic() +
    theme(legend.position = 'right')+
    guides(color = guide_legend(ncol = 1, title.position = "top"),
           linetype = guide_legend(ncol = 1, title.position = "top"))+
    ggtitle('Carbon Cap-and-Trade') +
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/system_emission_tradeoff_cap_v2.png'),width = 9,height=7)
}


for (i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_cost_vs_emission <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                             Subregions[i],'/LSE_Cost_Emission_Tradeoff',
                                             temp_total_title,".csv")) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
  cost_max <- max(lse_cost_vs_emission$`LSE Net Payment ($/MWh)`)
  cost_min <- min(lse_cost_vs_emission$`LSE Net Payment ($/MWh)`)
  ler_max <- max(lse_cost_vs_emission$`Load Emissions Rate (Ton/MWh)`[lse_cost_vs_emission$year == '2030'])
  ler2019 <- 0.466
  cost2019 <- 46.11
  
  Policy = rep('No Federal Policy',nrow(lse_cost_vs_emission))
  Policy[grep('Clean Energy Standard',lse_cost_vs_emission$Scenario)] = 
    'Clean Energy Standard';
  Policy[grep('Cap-and-Trade',lse_cost_vs_emission$Scenario)] = 
    'Carbon Cap-and-Trade';
  MajorTechSensitivity <- c('Mid',
                            'Low RE/BESS Cost', 
                            'Low NatGas Price',
                            'High RE/BESS Cost',
                            'High NatGas Price')      
  lse_cost_vs_emission_w_policy = cbind(lse_cost_vs_emission, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy')  
  cost_max <- max(lse_cost_vs_emission_w_policy$`LSE Net Payment ($/MWh)`)
  cost_min <- min(lse_cost_vs_emission_w_policy$`LSE Net Payment ($/MWh)`)
  ggplot() +
    annotate("rect", xmin = 0, ymin = 0, xmax = ler2019, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = ler2019/2, y = cost_min-0.5, label = "Emissions & LSE cost lower than 2019") +
    annotate("rect", xmin = 0.182, ymin = 0, xmax = 0.122, ymax = cost_max, fill = 'blue',alpha = 0.05)+
    annotate("text", x = 0.152, y = cost_max+0.5, label = "70%-80% reduction from 2005 level") +
    geom_point(data = lse_cost_vs_emission_w_policy, 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) + 
    geom_path(data = lse_cost_vs_emission_w_policy, 
              aes(y = `LSE Net Payment ($/MWh)`, 
                  x = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity),
              alpha = 0.3) + 
    facet_grid(.~Policy)+
    # geom_vline(data = filter(Reference_Emission,
    #                          Reference != '2019 Level (simulated)'), 
    #            aes(xintercept = `Load Emissions Rate (Ton/MWh)`, 
    #                linetype = Reference), 
    #            color = 'black',
    #            alpha = 0.3,
    #            show.legend = F) +
    # geom_text(data = filter(Reference_Emission,
    #                         Reference != '2019 Level (simulated)'), 
    #            aes(x = `Load Emissions Rate (Ton/MWh)`, 
    #                y = cost_max+0.5,
    #                label = Reference)) +
    geom_vline(xintercept = 0, color = 'red') +
    geom_vline(xintercept = ler2019, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = ler2019, y = cost2019) +
    annotate("text", x = ler2019 - 0.05, y = cost2019+1, label = "2019 level") +
    scale_color_brewer(palette = 'Set2') +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_x_reverse(limits = c(ler_max,0))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("Load Emission Rate (ton/MWh)")+
    theme(legend.position = 'bottom',
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank())+
    guides(color = guide_legend(nrow = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_emission_tradeoff_reverse_v2.png'),
           width = 12,
           height= 6)
}
