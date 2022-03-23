# CE Report theme plot 2 Clean energy share comparison
# Created by Qingyu Xu
# Created on Oct 6, 2021
settingfile <- 'postprocessing_inputs.csv';
RunFdr <-"/Users/qingyuxu/Documents/pjm_ce_all/"
source('./code/Header.R')
ScenarioFilter = c('Cap-and-Trade (40% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (45% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (85% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (100% Reduction Compare to 2005 Level)',
                   'Clean Energy Standard (40%)',
                   'Clean Energy Standard (45%)',
                   'Clean Energy Standard (100%)')
TechSensitivityColorCode = c(
  "Mid" = '#e31a1c',
  "Low RE/BESS Cost" = '#a6cee3',
  "High RE/BESS Cost" = '#1f78b4',
  "Low NatGas Price" = '#b2df8a',
  "High NatGas Price" = '#33a02c',
  "No Interregional Transmission Upgrade" = '#fdbf6f',
  "Half Interregional Transmission Upgrade" = '#ff7f00',
  "New Gas Capacity Caped at 20% of Existing" = '#cab2d6',
  "No New Gas Installation" = '#6a3d9a',
  "No Nuclear Retirement" = '#b15928',
  "New Gas Capacity Capped at 20% of Existing Gas" = '#cab2d6')
TechSensitivityLineType = c(
  "Mid" = 1,
  "Low RE/BESS Cost" = 2,
  "High RE/BESS Cost" = 2,
  "Low NatGas Price" = 2,
  "High NatGas Price" = 2,
  "No Interregional Transmission Upgrade" = 2,
  "Half Interregional Transmission Upgrade" = 2,
  "New Gas Capacity Caped at 20% of Existing" = 2,
  "No New Gas Installation" = 2,
  "No Nuclear Retirement" = 2,
  "New Gas Capacity Capped at 20% of Existing Gas" = 2)
tech_sensitivity <- c(tech_sensitivity,'New Gas Capacity Capped at 20% of Existing Gas')
for (i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_cost_vs_emission <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                          Subregions[i],'/LSE_Cost_Emission_Tradeoff',
                                          temp_total_title,".csv")) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
  
  cleanenergy <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                          Subregions[i],'/Generation/Gen_Output_',
                                          temp_total_title,".csv")) %>%
    filter(Fuel %in% na.omit(clean_fuel)) %>%
    group_by(year, Scenario, TechSensitivity) %>%
    summarize(TotalCEOutput = sum(AnnualOutput)/1e6) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))
  storage_loss <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                       Subregions[i],'/Generation/Stor_Operation_',
                                       temp_total_title,".csv")) %>%
    group_by(year, Scenario, TechSensitivity) %>%
    summarize(AnnualLoss = sum(AnnualLoss)) %>%
    select(year, Scenario, TechSensitivity, AnnualLoss)
  lse_clean_energy_share = left_join(lse_cost_vs_emission, cleanenergy) %>%
    left_join(storage_loss) %>%
    mutate(CEshare = round(TotalCEOutput*1e6/(`Gross Total` + AnnualLoss),2)) %>%
    filter(!(Scenario %in% ScenarioFilter))
  lse_clean_energy_share$TechSensitivity[which(lse_clean_energy_share$TechSensitivity == "New Gas Capacity Caped at 20% of Existing")] <- 'New Gas Capacity Capped at 20% of Existing Gas'
  cost_max <- max(lse_clean_energy_share$`LSE Net Payment ($/MWh)`)
  cost_min <- min(lse_clean_energy_share$`LSE Net Payment ($/MWh)`)
  ces_max <- max(lse_clean_energy_share$CEshare[lse_clean_energy_share$year == '2030'])
  ces2019 <- 0.40
  cost2019 <- 46.11
  
  Policy = rep('No Federal Policy',nrow(lse_clean_energy_share))
  Policy[grep('Clean Energy Standard',lse_clean_energy_share$Scenario)] = 
    'Clean Energy Standard';
  Policy[grep('Cap-and-Trade',lse_clean_energy_share$Scenario)] = 
    'Carbon Cap-and-Trade';
  MajorTechSensitivity <- c('Mid','Low RE/BESS Cost', 'Low NatGas Price', 
                            'High RE/BESS Cost','High NatGas Price')      
  lse_clean_energy_share_w_policy = cbind(lse_clean_energy_share, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(CEshare)
  # cost_max <- max(lse_clean_energy_share_w_policy$`LSE Net Payment ($/MWh)`)
  # cost_min <- min(lse_clean_energy_share_w_policy$`LSE Net Payment ($/MWh)`)
  ceshare_range <- lse_clean_energy_share_w_policy %>%
    group_by(Scenario) %>%
    summarise(lsemax = max(`LSE Net Payment ($/MWh)`),
              lsemin = min(`LSE Net Payment ($/MWh)`),
              xceshare = min(`CEshare`))
  ceshare_range_recost <- lse_clean_energy_share_w_policy %>%
    group_by(Scenario) %>%
    filter(TechSensitivity %in% c('Mid','Low RE/BESS Cost','High RE/BESS Cost'))%>%
    summarise(lsemax = max(`LSE Net Payment ($/MWh)`),
              lsemin = min(`LSE Net Payment ($/MWh)`),
              xler = min(`CEshare`))
  ceshare_range_ngprice <- lse_clean_energy_share_w_policy %>%
    group_by(Scenario) %>%
    filter(TechSensitivity %in% c('Mid','Low NatGas Price','High NatGas Price'))%>%
    summarise(lsemax = max(`LSE Net Payment ($/MWh)`),
              lsemin = min(`LSE Net Payment ($/MWh)`),
              xler = min(`CEshare`))
  ggplot() +
    annotate("rect", xmin = ces2019, ymin = 0, xmax = 1, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = (ces2019+1)/2, y = cost_min+1, label = "Lower LSE cost & \nmore clean power than 2019") +
    # annotate("rect", xmin = 0.182, ymin = 0, xmax = 0.122, ymax = cost_max, fill = 'blue',alpha = 0.05)+
    # annotate("text", x = 0.152, y = cost_max+0.5, label = "70%-80% reduction from 2005 level") +
    geom_point(data = lse_clean_energy_share_w_policy, 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `CEshare`,
                   color = TechSensitivity)) + 
    geom_path(data = lse_clean_energy_share_w_policy, 
              aes(y = `LSE Net Payment ($/MWh)`, 
                  x = `CEshare`,
                  color = TechSensitivity),
              alpha = 0.3) + 
    facet_grid(.~Policy)+
    geom_vline(xintercept = 1, color = 'red') +
    geom_vline(xintercept = 0.4, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = 0.4, y = cost2019) +
    annotate("text", x = 0.4+0.07, y = cost2019+1, label = "2019 level") +
    scale_color_brewer(palette = 'Set2') +
    coord_cartesian(ylim = c(cost_min, cost_max),
                    xlim = c(0.2,1))+
    # scale_x_reverse(limits = c(ler_max,0))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("[Total Clean Energy]/[Total Load]")+
    theme(legend.position = 'bottom',
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank())+
    guides(color = guide_legend(nrow = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_ces_tradeoff_reverse_v2.png'),
           width = 12,
           height= 6)
  ggplot() +
    annotate("rect", xmin = ces2019, ymin = 0, xmax = 1, ymax = cost2019, fill = 'green',alpha = 0.02)+
    annotate("text", x = (ces2019+1)/2, y = cost_min, label = "Lower LSE cost & higher clean energy share than 2019") +
    geom_vline(xintercept = 1, color = 'red') +
    geom_vline(xintercept = 0.4, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = 0.4, y = cost2019) +
    annotate("text", x = 0.4+0.07, y = cost2019+1, label = "2019 level") +
    # annotate("rect", xmin = 0.182, ymin = 0, xmax = 0.122, ymax = cost_max, fill = 'blue',alpha = 0.05)+
    # annotate("text", x = 0.152, y = cost_max+0.5, label = "70%-80% reduction from 2005 level") +
    geom_linerange(data = filter(ceshare_range,
                                 grepl('Clean Energy Standard',Scenario)),
                   aes(x = xceshare, ymin = lsemin, ymax = lsemax),
                   color = 'black',
                   alpha = 0.5)+
    geom_ribbon(data = filter(ceshare_range_ngprice,
                              grepl('Clean Energy Standard',Scenario)),
                aes(x = xler, ymin = lsemin, ymax = lsemax),
                color = '#2ca24f',alpha = 0.5,fill = '#e5f5f9') +
    geom_ribbon(data = filter(ceshare_range_recost,
                              grepl('Clean Energy Standard',Scenario)),
                aes(x = xler, ymin = lsemin, ymax = lsemax),
                color = '#08419c',alpha = 0.5,fill = '#eff3ff') +
    geom_point(data = filter(lse_clean_energy_share_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `CEshare`,
                   color = TechSensitivity)) + 
    geom_point(data = filter(lse_clean_energy_share_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity == 'Mid'),
               aes(y = `LSE Net Payment ($/MWh)`,
                   x = `CEshare`,
                   color = TechSensitivity),
               size = 3) +
    # facet_grid(.~Policy)+
    scale_color_manual(values = TechSensitivityColorCode) +
    coord_cartesian(ylim = c(cost_min, cost_max),
                    xlim = c(0.2,1))+
    scale_x_continuous(labels =scales::percent, breaks = seq(0.2,1,0.1))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("% of total load supported by clean energy")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_ces_tradeoff_reverse_v2_cesonly.png'),
           width = 7,
           height= 6)
  
  MajorTechSensitivity <- c('Mid','No Nuclear Retirement')
  lse_clean_energy_share_w_policy = cbind(lse_clean_energy_share, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(CEshare)
  ggplot() +
    annotate("rect", xmin = ces2019, ymin = 0, xmax = 1, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = (ces2019+1)/2, y = cost_min+1, label = "Lower LSE cost & \nmore clean power than 2019") +
    # annotate("rect", xmin = 0.182, ymin = 0, xmax = 0.122, ymax = cost_max, fill = 'blue',alpha = 0.05)+
    # annotate("text", x = 0.152, y = cost_max+0.5, label = "70%-80% reduction from 2005 level") +
    geom_point(data = filter(lse_clean_energy_share_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity == 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `CEshare`,
                   color = TechSensitivity),
               size = 3) + 
    geom_point(data = filter(lse_clean_energy_share_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `CEshare`,
                   color = TechSensitivity)) + 
    geom_path(data = filter(lse_clean_energy_share_w_policy,
                            Policy == 'Clean Energy Standard'), 
              aes(y = `LSE Net Payment ($/MWh)`, 
                  x = `CEshare`,
                  color = TechSensitivity),
              alpha = 0.3) + 
    geom_vline(xintercept = 1, color = 'red') +
    geom_vline(xintercept = 0.4, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = 0.4, y = cost2019) +
    annotate("text", x = 0.4+0.07, y = cost2019+1, label = "2019 level") +
    scale_color_manual(values = TechSensitivityColorCode) +
    scale_x_continuous(labels =scales::percent, breaks = seq(0.2,1,0.1))+
    coord_cartesian(ylim = c(cost_min, cost_max),
                    xlim = c(0.2,1))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("% of total load supported by clean energy")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_ces_tradeoff_reverse_v2_nuclearimpact.png'),
           width = 7,
           height= 6)
  
  MajorTechSensitivity <- c('Mid','No New Gas Installation','New Gas Capacity Capped at 20% of Existing Gas')  
  lse_clean_energy_share_w_policy = cbind(lse_clean_energy_share, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(CEshare)
  ggplot() +
    annotate("rect", xmin = ces2019, ymin = 0, xmax = 1, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = (ces2019+1)/2, y = cost_min+1, label = "Lower LSE cost & \nmore clean power than 2019") +
    # annotate("rect", xmin = 0.182, ymin = 0, xmax = 0.122, ymax = cost_max, fill = 'blue',alpha = 0.05)+
    # annotate("text", x = 0.152, y = cost_max+0.5, label = "70%-80% reduction from 2005 level") +
    geom_point(data = filter(lse_clean_energy_share_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity == 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `CEshare`,
                   color = TechSensitivity),
               size = 3) + 
    geom_point(data = filter(lse_clean_energy_share_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `CEshare`,
                   color = TechSensitivity)) + 
    geom_path(data = filter(lse_clean_energy_share_w_policy,
                            Policy == 'Clean Energy Standard'), 
              aes(y = `LSE Net Payment ($/MWh)`, 
                  x = `CEshare`,
                  color = TechSensitivity),
              alpha = 0.3) + 
    geom_vline(xintercept = 1, color = 'red') +
    geom_vline(xintercept = 0.4, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = 0.4, y = cost2019) +
    annotate("text", x = 0.4+0.07, y = cost2019+1, label = "2019 level") +
    scale_color_manual(values = TechSensitivityColorCode) +
    scale_x_continuous(labels =scales::percent, breaks = seq(0.2,1,0.1))+
    coord_cartesian(ylim = c(cost_min, cost_max),
                    xlim = c(0.2,1))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("% of total load supported by clean energy")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_ces_tradeoff_reverse_v2_natgasimpact.png'),
           width = 7,
           height= 6)
  MajorTechSensitivity <- c('Mid','No Interregional Transmission Upgrade')  
  lse_clean_energy_share_w_policy = cbind(lse_clean_energy_share, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(CEshare)
  ggplot() +
    annotate("rect", xmin = ces2019, ymin = 0, xmax = 1, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = (ces2019+1)/2, y = cost_min+1, label = "Lower LSE cost & \nmore clean power than 2019") +
    # annotate("rect", xmin = 0.182, ymin = 0, xmax = 0.122, ymax = cost_max, fill = 'blue',alpha = 0.05)+
    # annotate("text", x = 0.152, y = cost_max+0.5, label = "70%-80% reduction from 2005 level") +
    geom_point(data = filter(lse_clean_energy_share_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity == 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `CEshare`,
                   color = TechSensitivity),
               size = 3) + 
    geom_point(data = filter(lse_clean_energy_share_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `CEshare`,
                   color = TechSensitivity)) + 
    geom_path(data = filter(lse_clean_energy_share_w_policy,
                            Policy == 'Clean Energy Standard'), 
              aes(y = `LSE Net Payment ($/MWh)`, 
                  x = `CEshare`,
                  color = TechSensitivity),
              alpha = 0.3) + 
    geom_vline(xintercept = 1, color = 'red') +
    geom_vline(xintercept = 0.4, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = 0.4, y = cost2019) +
    annotate("text", x = 0.4+0.07, y = cost2019+1, label = "2019 level") +
    scale_color_manual(values = TechSensitivityColorCode) +
    scale_x_continuous(labels =scales::percent, breaks = seq(0.2,1,0.1))+
    coord_cartesian(ylim = c(cost_min, cost_max),
                    xlim = c(0.2,1))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("% of total load supported by clean energy")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_ces_tradeoff_reverse_v2_transmissionimpact.png'),
           width = 7,
           height= 6)
}
