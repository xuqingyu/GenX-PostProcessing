# CE Report theme plot 1 Emission comparison
# Created by Qingyu Xu
# Created on Oct 6, 2021
settingfile <- 'postprocessing_inputs.csv';
RunFdr <-"/Users/qingyuxu/Documents/pjm_ce_all/"
source('./code/Header.R')
# Reference_Emission <- as_tibble(cbind(`Load Emissions Rate (Ton/MWh)` = c(0.466, 0.304, 0.243, 0.182, 0.152, 0.122, 0.061),
#                                 Reference = c('2019 Level (simulated)','50% Emission Reduction of 2005 level',
#                                                     '60% Emission Reduction','70% Emission Reduction','75% Emission Reduction',
#                                                     '80% Emission Reduction','90% Emission Reduction')))
Reference_Emission <- as_tibble(cbind(`Load Emissions Rate (Ton/MWh)` = c(0.466, 0.304, 0.122),
                                      Reference = c('2019 Level (simulated)','50% of 2005 level',
                                                    '20% of 2005 level')))
Reference_Emission$`Load Emissions Rate (Ton/MWh)` = as.numeric(Reference_Emission$`Load Emissions Rate (Ton/MWh)`)
ScenarioFilter = c('Cap-and-Trade (40% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (45% Reduction Compare to 2005 Level)',
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
  "New Gas Capacity Capped at 20% of Existing" = '#cab2d6',
  "No New Gas Installation" = '#6a3d9a',
  "No Nuclear Retirement" = '#b15928',
  "New Gas Capacity Capped at 20% of Existing Gas" = '#cab2d6',
  "Allow CCS Expansion" = '#cab2d6')
TechSensitivityLineType = c(
  "Mid" = 1,
  "Low RE/BESS Cost" = 2,
  "High RE/BESS Cost" = 2,
  "Low NatGas Price" = 2,
  "High NatGas Price" = 2,
  "No Interregional Transmission Upgrade" = 2,
  "Half Interregional Transmission Upgrade" = 2,
  "New Gas Capacity Capped at 20% of Existing" = 2,
  "No New Gas Installation" = 2,
  "No Nuclear Retirement" = 2,
  "New Gas Capacity Capped at 20% of Existing Gas" = 2,
  "Allow CCS Expansion" = 2)
#tech_sensitivity <- c(tech_sensitivity,'New Gas Capacity Capped at 20% of Existing Gas')

for (i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  storage_loss <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                  Subregions[i],'/Generation/Stor_Operation_',
                                  temp_total_title,".csv")) %>%
    group_by(year, Scenario, TechSensitivity) %>%
    summarize(AnnualLoss = sum(AnnualLoss)) %>%
    select(year, Scenario, TechSensitivity, AnnualLoss)
  lse_cost_vs_emission <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                          Subregions[i],'/LSE_Cost_Emission_Tradeoff',
                                          temp_total_title,".csv")) %>%
    left_join(storage_loss) %>%
    mutate(`Load Emissions Rate (Ton/MWh)` = `Emissions (Mtons)`*1e6/(`Gross Total` + AnnualLoss)) %>%
    mutate(Scenario = factor(Scenario, levels = scenario),
           TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity)) %>%
    filter(!(Scenario %in% ScenarioFilter))
  # lse_cost_vs_emission$TechSensitivity[which(lse_cost_vs_emission$TechSensitivity == "New Gas Capacity Caped at 20% of Existing")] <- 'New Gas Capacity Capped at 20% of Existing Gas'
  cost_max <- max(lse_cost_vs_emission$`LSE Net Payment ($/MWh)`)
  cost_min <- min(lse_cost_vs_emission$`LSE Net Payment ($/MWh)`)
  # ler_max <- max(lse_cost_vs_emission$`Load Emissions Rate (Ton/MWh)`[lse_cost_vs_emission$year == '2030'])
  ler_max = 0.5;
  ler2019 <- 0.466
  cost2019 <- 46.11
  
  Policy = rep('No Federal Policy',nrow(lse_cost_vs_emission))
  Policy[grep('Clean Energy Standard',lse_cost_vs_emission$Scenario)] = 
    'Clean Energy Standard';
  Policy[grep('Cap-and-Trade',lse_cost_vs_emission$Scenario)] = 
    'Carbon Cap-and-Trade';
  MajorTechSensitivity <- c('Mid','Low RE/BESS Cost', 'Low NatGas Price',
                            'High RE/BESS Cost','High NatGas Price')    
  lse_cost_vs_emission_w_policy = cbind(lse_cost_vs_emission, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(`Load Emissions Rate (Ton/MWh)`)
  arrowstart = lse_cost_vs_emission_w_policy %>%
    filter(TechSensitivity == 'Mid') %>%
    select(Scenario, TechSensitivity, Policy, `Load Emissions Rate (Ton/MWh)`, `LSE Net Payment ($/MWh)`) %>%
    rename(xstart = `Load Emissions Rate (Ton/MWh)`, ystart = `LSE Net Payment ($/MWh)`) %>%
    select(-TechSensitivity)
  arrowend = lse_cost_vs_emission_w_policy %>%
    filter(TechSensitivity != 'Mid') %>%
    select(Scenario, TechSensitivity, Policy, `Load Emissions Rate (Ton/MWh)`, `LSE Net Payment ($/MWh)`) %>%
    rename(xend = `Load Emissions Rate (Ton/MWh)`, yend = `LSE Net Payment ($/MWh)`)
  arrow = left_join(arrowstart, arrowend)
  ggplot() +
    annotate("rect", xmin = 0, ymin = 0, xmax = ler2019, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = ler2019/2, y = cost_min-0.5, label = "Emissions & LSE cost lower than 2019") +
    geom_segment(data = filter(arrow, 
                               Policy == 'Clean Energy Standard', 
                               TechSensitivity %in% c('Low RE/BESS Cost', 'High RE/BESS Cost'),
                               Scenario %in% c('Clean Energy Standard (50%)','Clean Energy Standard (95%)')),
                 aes(x = xstart, xend = xend,y = ystart, yend = yend),
                 color = '#08419c',arrow = arrow(length = unit(0.02, "npc")))+
    geom_segment(data = filter(arrow, Policy == 'Clean Energy Standard', 
                               TechSensitivity %in% c('Low NatGas Price', 'High NatGas Price'),
                               Scenario %in% c('Clean Energy Standard (50%)','Clean Energy Standard (95%)')),
                 aes(x = xstart, xend = xend,y = ystart, yend = yend),
                 color = '#2ca24f',arrow = arrow(length = unit(0.02, "npc")))+
    geom_path(data = filter(lse_cost_vs_emission_w_policy,
                            Policy == 'Clean Energy Standard'),
              aes(y = `LSE Net Payment ($/MWh)`,
                  x = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity),
              alpha = 0.3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity == 'Mid'),
               aes(y = `LSE Net Payment ($/MWh)`,
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity),
               size = 3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Clean Energy Standard',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) +
    # facet_wrap(.~Policy, strip.position = 'right')+
    scale_color_manual(values = TechSensitivityColorCode) +
    geom_vline(xintercept = 0, color = 'red') +
    geom_vline(xintercept = ler2019, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = ler2019, y = cost2019) +
    annotate("text", x = ler2019 - 0.05, y = cost2019+1, label = "2019 level") +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_x_reverse(limits = c(ler_max,0),
                    sec.axis = sec_axis(~ (1-./0.607), labels = scales::percent,
                                        name = '% reduction from 2005 emissions level = 0.607 ton/MWh',
                                        breaks = seq(from = 0, to = 1, by = 0.1)))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("Load Emission Rate (ton/MWh)")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_emission_tradeoff_reverse_ces.png'),
           width = 7,
           height= 6)
  ggplot() +
    annotate("rect", xmin = 0, ymin = 0, xmax = ler2019, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = ler2019/2, y = cost_min-0.5, label = "Emissions & LSE cost lower than 2019") +
    geom_path(data = filter(lse_cost_vs_emission_w_policy,
                            Policy == 'Carbon Cap-and-Trade'),
              aes(y = `LSE Net Payment ($/MWh)`,
                  x = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity),
              alpha = 0.3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity == 'Mid'),
               aes(y = `LSE Net Payment ($/MWh)`,
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity),
               size = 3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) +
    # facet_wrap(.~Policy, strip.position = 'right')+
    scale_color_manual(values = TechSensitivityColorCode) +
    geom_vline(xintercept = 0, color = 'red') +
    geom_vline(xintercept = ler2019, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = ler2019, y = cost2019) +
    annotate("text", x = ler2019 - 0.05, y = cost2019+1, label = "2019 level") +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_x_reverse(limits = c(ler_max,0),
                    sec.axis = sec_axis(~ (1-./0.607), labels = scales::percent,
                                        name = '% reduction from 2005 emissions level = 0.607 ton/MWh',
                                        breaks = seq(from = 0, to = 1, by = 0.1)))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("Load Emission Rate (ton/MWh)")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_emission_tradeoff_reverse_cat.png'),
           width = 7,
           height= 6)
  
  lse_range <- lse_cost_vs_emission_w_policy %>%
    group_by(Scenario) %>%
    summarise(lsemax = max(`LSE Net Payment ($/MWh)`),
              lsemin = min(`LSE Net Payment ($/MWh)`),
              xler = max(`Load Emissions Rate (Ton/MWh)`))
  lse_range_mid <- lse_cost_vs_emission_w_policy %>%
    group_by(Scenario) %>%
    filter(TechSensitivity %in% c('Mid'))%>%
    summarise(lsemidpoint = max(`LSE Net Payment ($/MWh)`))
  lse_range_recost <- lse_cost_vs_emission_w_policy %>%
    group_by(Scenario) %>%
    filter(TechSensitivity %in% c('Mid','Low RE/BESS Cost','High RE/BESS Cost'))%>%
    summarise(lsemax = max(`LSE Net Payment ($/MWh)`),
              lsemin = min(`LSE Net Payment ($/MWh)`),
              xler = max(`Load Emissions Rate (Ton/MWh)`))
  lse_range_ngprice <- lse_cost_vs_emission_w_policy %>%
    group_by(Scenario) %>%
    filter(TechSensitivity %in% c('Mid','Low NatGas Price','High NatGas Price'))%>%
    summarise(lsemax = max(`LSE Net Payment ($/MWh)`),
              lsemin = min(`LSE Net Payment ($/MWh)`),
              xler = max(`Load Emissions Rate (Ton/MWh)`))
  
  ggplot() +
    annotate("rect", xmin = 0, ymin = 0, xmax = ler2019, ymax = cost2019, fill = 'green',alpha = 0.02)+
    annotate("text", x = ler2019/2, y = cost_min-0.5, label = "Emissions & LSE cost lower than 2019") +
    geom_vline(xintercept = 0, color = 'red') +
    geom_vline(xintercept = ler2019, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = ler2019, y = cost2019) +
    annotate("text", x = ler2019 - 0.05, y = cost2019+1, label = "2019 level") +
    geom_linerange(data = filter(lse_range,
                                 grepl('Cap-and-Trade',Scenario)),
                   aes(x = xler, ymin = lsemin, ymax = lsemax),
                   color = 'black',
                   alpha = 0.5)+
    geom_ribbon(data = filter(lse_range_ngprice,
                              grepl('Cap-and-Trade',Scenario)),
                aes(x = xler, ymin = lsemin, ymax = lsemax),
                color = '#2ca24f',alpha = 0.5,fill = '#e5f5f9') +
    geom_ribbon(data = filter(lse_range_recost,
                              grepl('Cap-and-Trade',Scenario)),
                aes(x = xler, ymin = lsemin, ymax = lsemax),
                color = '#08419c',alpha = 0.5,fill = '#eff3ff') +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity == 'Mid'),
               aes(y = `LSE Net Payment ($/MWh)`,
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity),
               size = 3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) +
    scale_color_manual(values = TechSensitivityColorCode) +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_y_continuous(breaks = seq(0,130,10))+
    scale_x_reverse(limits = c(ler_max,0),
                    sec.axis = sec_axis(~ (1-./0.607), labels = scales::percent,
                                        name = '% reduction from 2005 emissions level = 0.607 ton/MWh',
                                        breaks = seq(from = 0, to = 1, by = 0.1)))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("Load Emission Rate (ton/MWh)")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_emission_tradeoff_reverse_v2_caponly.png'),
           width = 7,
           height= 6)
  
  
  MajorTechSensitivity <- c('Mid','No Nuclear Retirement')    
  lse_cost_vs_emission_w_policy = cbind(lse_cost_vs_emission, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(`Load Emissions Rate (Ton/MWh)`)
  ggplot() +
    annotate("rect", xmin = 0, ymin = 0, xmax = ler2019, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = ler2019/2, y = cost_min-0.5, label = "Emissions & LSE cost lower than 2019") +
    geom_path(data = filter(lse_cost_vs_emission_w_policy,
                            Policy == 'Carbon Cap-and-Trade'),
              aes(y = `LSE Net Payment ($/MWh)`,
                  x = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity),
              alpha = 0.3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity == 'Mid'),
               aes(y = `LSE Net Payment ($/MWh)`,
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity),
               size = 3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) +
    # facet_wrap(.~Policy, strip.position = 'right')+
    scale_color_manual(values = TechSensitivityColorCode) +
    geom_vline(xintercept = 0, color = 'red') +
    geom_vline(xintercept = ler2019, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = ler2019, y = cost2019) +
    annotate("text", x = ler2019 - 0.05, y = cost2019+1, label = "2019 level") +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_x_reverse(limits = c(ler_max,0),
                    sec.axis = sec_axis(~ (1-./0.607), labels = scales::percent,
                                        name = '% reduction from 2005 emissions level = 0.607 ton/MWh',
                                        breaks = seq(from = 0, to = 1, by = 0.1)))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("Load Emission Rate (ton/MWh)")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_emission_tradeoff_reverse_cat_nuclearimpact.png'),
           width = 7,
           height= 6)
  
  MajorTechSensitivity <- c('Mid','No New Gas Installation')    
  lse_cost_vs_emission_w_policy = cbind(lse_cost_vs_emission, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(`Load Emissions Rate (Ton/MWh)`)
  ggplot() +
    annotate("rect", xmin = 0, ymin = 0, xmax = ler2019, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = ler2019/2, y = cost_min-0.5, label = "Emissions & LSE cost lower than 2019") +
    geom_path(data = filter(lse_cost_vs_emission_w_policy,
                            Policy == 'Carbon Cap-and-Trade'),
              aes(y = `LSE Net Payment ($/MWh)`,
                  x = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity),
              alpha = 0.3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity == 'Mid'),
               aes(y = `LSE Net Payment ($/MWh)`,
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity),
               size = 3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) +
    # facet_wrap(.~Policy, strip.position = 'right')+
    scale_color_manual(values = TechSensitivityColorCode) +
    geom_vline(xintercept = 0, color = 'red') +
    geom_vline(xintercept = ler2019, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = ler2019, y = cost2019) +
    annotate("text", x = ler2019 - 0.05, y = cost2019+1, label = "2019 level") +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_x_reverse(limits = c(ler_max,0),
                    sec.axis = sec_axis(~ (1-./0.607), labels = scales::percent,
                                        name = '% reduction from 2005 emissions level = 0.607 ton/MWh',
                                        breaks = seq(from = 0, to = 1, by = 0.1)))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("Load Emission Rate (ton/MWh)")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_emission_tradeoff_reverse_cat_natgasimpact.png'),
           width = 7,
           height= 6)
  
  MajorTechSensitivity <- c('Mid','No Interregional Transmission Upgrade')    
  lse_cost_vs_emission_w_policy = cbind(lse_cost_vs_emission, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(`Load Emissions Rate (Ton/MWh)`)
  ggplot() +
    annotate("rect", xmin = 0, ymin = 0, xmax = ler2019, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = ler2019/2, y = cost_min-0.5, label = "Emissions & LSE cost lower than 2019") +
    geom_path(data = filter(lse_cost_vs_emission_w_policy,
                            Policy == 'Carbon Cap-and-Trade'),
              aes(y = `LSE Net Payment ($/MWh)`,
                  x = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity),
              alpha = 0.3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity == 'Mid'),
               aes(y = `LSE Net Payment ($/MWh)`,
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity),
               size = 3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) +
    # facet_wrap(.~Policy, strip.position = 'right')+
    scale_color_manual(values = TechSensitivityColorCode) +
    geom_vline(xintercept = 0, color = 'red') +
    geom_vline(xintercept = ler2019, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = ler2019, y = cost2019) +
    annotate("text", x = ler2019 - 0.05, y = cost2019+1, label = "2019 level") +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_x_reverse(limits = c(ler_max,0),
                    sec.axis = sec_axis(~ (1-./0.607), labels = scales::percent,
                                        name = '% reduction from 2005 emissions level = 0.607 ton/MWh',
                                        breaks = seq(from = 0, to = 1, by = 0.1)))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("Load Emission Rate (ton/MWh)")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_emission_tradeoff_reverse_cat_transmissionimpact.png'),
           width = 7,
           height= 6)
  
  
  MajorTechSensitivity <- c('Mid','Allow CCS Expansion')    
  lse_cost_vs_emission_w_policy = cbind(lse_cost_vs_emission, Policy) %>%
    filter(year == '2030',
           TechSensitivity %in% MajorTechSensitivity,
           Policy != 'No Federal Policy') %>%
    arrange(`Load Emissions Rate (Ton/MWh)`)
  ggplot() +
    annotate("rect", xmin = 0, ymin = 0, xmax = ler2019, ymax = cost2019, fill = 'green',alpha = 0.05)+
    annotate("text", x = ler2019/2, y = cost_min-0.5, label = "Emissions & LSE cost lower than 2019") +
    geom_path(data = filter(lse_cost_vs_emission_w_policy,
                            Policy == 'Carbon Cap-and-Trade'),
              aes(y = `LSE Net Payment ($/MWh)`,
                  x = `Load Emissions Rate (Ton/MWh)`,
                  color = TechSensitivity),
              alpha = 0.3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity == 'Mid'),
               aes(y = `LSE Net Payment ($/MWh)`,
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity),
               size = 3) +
    geom_point(data = filter(lse_cost_vs_emission_w_policy,
                             Policy == 'Carbon Cap-and-Trade',
                             TechSensitivity != 'Mid'), 
               aes(y = `LSE Net Payment ($/MWh)`, 
                   x = `Load Emissions Rate (Ton/MWh)`,
                   color = TechSensitivity)) +
    # facet_wrap(.~Policy, strip.position = 'right')+
    scale_color_manual(values = TechSensitivityColorCode) +
    geom_vline(xintercept = 0, color = 'red') +
    geom_vline(xintercept = ler2019, color = 'black') +
    geom_hline(yintercept = cost2019) +
    annotate("point", x = ler2019, y = cost2019) +
    annotate("text", x = ler2019 - 0.05, y = cost2019+1, label = "2019 level") +
    coord_cartesian(ylim = c(cost_min, cost_max))+
    scale_x_reverse(limits = c(ler_max,0),
                    sec.axis = sec_axis(~ (1-./0.607), labels = scales::percent,
                                        name = '% reduction from 2005 emissions level = 0.607 ton/MWh',
                                        breaks = seq(from = 0, to = 1, by = 0.1)))+
    theme_classic2() +
    ylab("LSE Cost ($/MWh)") +
    xlab("Load Emission Rate (ton/MWh)")+
    theme(legend.position = c(0.5,0.8),
          legend.key = element_rect(fill = "white", colour = "black"),
          legend.title = element_blank(),
          axis.line = element_line(colour = "black",size=0),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=1.5))+
    guides(color = guide_legend(ncol = 1, title.position = "top"))+
    ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[i],
                  '/Graphics/lsecost_emission_tradeoff_reverse_cat_ccsimpact.png'),
           width = 7,
           height= 6)
}
