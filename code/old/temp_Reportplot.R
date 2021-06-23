source('./code/Header.R')
systemview_fn <- paste0(RunFdr, "/CompiledResults/NJ_SystemView_TriCore.csv");
if (file.exists(systemview_fn)){
  SystemView <- read_csv(systemview_fn)
  }

SystemView$`Tech Sensitivity` = factor(SystemView$`Tech Sensitivity`, levels = c('Mid', 
                                                                                 'Low RE Cost',
                                                                                 'High RE Cost',
                                                                                 'Low NG Price', 
                                                                                 'High NG Price',
                                                                                 'No Inter-ISO Transmission',
                                                                                 'Restrictive CCS Development',
                                                                                 'Double ZCF'))
for (y in unique(SystemView$year)) {
  tempdata <- SystemView %>% filter(year ==y) 
  ggplot(data = tempdata, aes(x=`Load Emission Rate (Metric ton/MWh)`,y = `System Cost ($/MWh)`,color = Scenario,size= 6))+
    geom_point(aes(shape = `Tech Sensitivity`)) +
    scale_shape_manual(values=c(2,15,22,16,21,3,4,5)) + 
    xlim(c(0,0.2))+
    ylim(c(45,80))+
    scale_x_reverse()+
    ggsave(paste0(RunFdr,'/Graphics/System Emission Trade-Off in Year ',y,'.png'),width = 10,height=10)
}


genoutput_fn <- paste0(RunFdr, "/CompiledResults/NJ_GenOutputMix.csv");
if (file.exists(genoutput_fn)){
  genoutput <- read_csv(genoutput_fn)
}

genoutput$`Tech Sensitivity` = factor(genoutput$`Tech Sensitivity`, levels = c('Mid', 
                                                                               'Low RE Cost',
                                                                               'High RE Cost',
                                                                               'Low NG Price', 
                                                                               'High NG Price',
                                                                               'No Inter-ISO Transmission',
                                                                               'Restrictive CCS Development',
                                                                               'Double ZCF'))
genoutput <- genoutput %>% pivot_longer(c('Solar',	'Offshore Wind',	'Onshore Wind',	'Hydro',	'Biomass',	'Nuclear',	'Gas CCS',	'Gas CC',	'Gas CT',	'Gas steam',	'Coal',	'Gas CC ZCF',	'Gas CT ZCF',	'Load')) %>%
  rename( `GenType/Load`=name, `TWh`= value, Year = year)
genoutput$`GenType/Load` <- factor(genoutput$`GenType/Load`,levels = c('Solar',	'Offshore Wind',	'Onshore Wind',	'Hydro',	'Biomass',	'Nuclear',	'Gas CCS',	'Gas CC',	'Gas CT',	'Gas steam',	'Coal',	'Gas CC ZCF',	'Gas CT ZCF',	'Load'))
genoutput$Scenario <- factor(genoutput$Scenario, levels = c('Current Policy','Stated Policy','Deep Decarbonization'));
ggplot(data = genoutput, aes(x=`Year`,y = `TWh`,fill = `GenType/Load`))+
  geom_col() +
  facet_grid(Scenario~`Tech Sensitivity`) + 
  scale_fill_manual(values = c('#fccb6e',
    '#92CF72',
    '#33A02C',
    '#408DBF',
    '#FFDAB9',
    '#E94330',
    '#DDA0DD',
    '#B294C7',
    '#6A3D9A',
    '#2F4F4F',
    '#696969',
    '#cd51e0',
    '#ff00cc',
    '#3CAEA3'))+ # for load
  ggsave(paste0(RunFdr,'/Graphics/GenvsLoad.png'),width = 20,height=10)


gencap_fn <- paste0(RunFdr, "/CompiledResults/NJ_GenCapMix.csv");
if (file.exists(gencap_fn)){
  gencap<- read_csv(gencap_fn)
}

gencap$`Tech Sensitivity` = factor(gencap$`Tech Sensitivity`, levels = c('Mid', 
                                                                               'Low RE Cost',
                                                                               'High RE Cost',
                                                                               'Low NG Price', 
                                                                               'High NG Price',
                                                                               'No Inter-ISO Transmission',
                                                                               'Restrictive CCS Development',
                                                                               'Double ZCF'))
gencap <- gencap %>% pivot_longer(c('Solar',	'Offshore Wind',	'Onshore Wind',	'Hydro',	'Biomass',	'Nuclear',	'Gas CCS',	'Gas CC',	'Gas CT',	'Gas steam',	'Coal',	'Gas CC ZCF',	'Gas CT ZCF')) %>%
  rename( `GenType`=name, `GW`= value, Year = year)
gencap$`GenType` <- factor(gencap$`GenType`,levels = c('Solar',	'Offshore Wind',	'Onshore Wind',	'Hydro',	'Biomass',	'Nuclear',	'Gas CCS',	'Gas CC',	'Gas CT',	'Gas steam',	'Coal',	'Gas CC ZCF',	'Gas CT ZCF',	'Load'))
gencap$Scenario <- factor(gencap$Scenario, levels = c('Current Policy','Stated Policy','Deep Decarbonization'));
ggplot(data = gencap, aes(x=`Year`,y = `GW`,fill = `GenType`))+
  geom_col() +
  facet_grid(Scenario~`Tech Sensitivity`) + 
  scale_fill_manual(values = c('#fccb6e',
                               '#92CF72',
                               '#33A02C',
                               '#408DBF',
                               '#FFDAB9',
                               '#E94330',
                               '#DDA0DD',
                               '#B294C7',
                               '#6A3D9A',
                               '#2F4F4F',
                               '#696969',
                               '#cd51e0',
                               '#ff00cc'))+
  ggsave(paste0(RunFdr,'/Graphics/GenCap.png'),width = 20,height=10)


regioncost_fn <- paste0(RunFdr, "/CompiledResults/NJ_RegionalCostBreakDown.csv");
if (file.exists(regioncost_fn)){
  regioncost<- read_csv(regioncost_fn)
}

regioncost$`Tech Sensitivity` = factor(regioncost$`Tech Sensitivity`, levels = c('Mid', 
                                                                         'Low RE Cost',
                                                                         'High RE Cost',
                                                                         'Low NG Price', 
                                                                         'High NG Price',
                                                                         'No Inter-ISO Transmission',
                                                                         'Restrictive CCS Development',
                                                                         'Double ZCF'))
regioncost <- regioncost %>% pivot_longer(c('Energy Market',	'Capacity Market',	'RGGI & CO2 Cap',	'RPS',	'NSE',	'Transmission Cost Allocation',
                                            'Congestion Revenue Allocation',	'Fuel & VOM',	'FOM',	'Incremental Investment',	'Sunk',	'Emissions Capturing')) %>%
  rename( `CostType`=name, `2020USD`= value, Year = year) %>%
  mutate(`2020USD/MWh`=`2020USD`/`Total Load`)
regioncost$`CostType` <- factor(regioncost$`CostType`,levels = c('Energy Market',	'Capacity Market',	'RGGI & CO2 Cap',	'RPS',	'NSE',	'Transmission Cost Allocation',
                                                                 'Congestion Revenue Allocation',	'Fuel & VOM',	'FOM',	'Incremental Investment',	'Sunk',	'Emissions Capturing'))
regioncost$Scenario <- factor(regioncost$Scenario, levels = c('Current Policy','Stated Policy','Deep Decarbonization'));
regioncost_total <- regioncost %>% group_by(Scenario,`Tech Sensitivity`,Year) %>% summarize(`2020USD/MWh` = sum(`2020USD/MWh`));
ggplot(data = regioncost, aes(x=`Year`,y = `2020USD/MWh`))+
  geom_col(aes(fill = `CostType`))+
  scale_fill_manual(values = c('#fccb6e',
                               '#92CF72',
                               '#33A02C',
                               '#408DBF',
                               '#FFDAB9',
                               '#E94330',
                               '#DDA0DD',
                               '#B294C7',
                               '#6A3D9A',
                               '#2F4F4F',
                               '#696969',
                               '#cd51e0'))+
  geom_point(data=regioncost_total, aes(x=`Year`,y = `2020USD/MWh`)) + 
  facet_grid(Scenario~`Tech Sensitivity`) + 
  ggsave(paste0(RunFdr,'/Graphics/RegionalCostBreakDown.png'),width = 20,height=10)




lsecost_fn <- paste0(RunFdr, "/CompiledResults/NJ_LSECost.csv");
if (file.exists(lsecost_fn)){
  lsecost<- read_csv(lsecost_fn)
}

lsecost$`Tech Sensitivity` = factor(lsecost$`Tech Sensitivity`, levels = c('Mid', 
                                                                                 'Low RE Cost',
                                                                                 'High RE Cost',
                                                                                 'Low NG Price', 
                                                                                 'High NG Price',
                                                                                 'No Inter-ISO Transmission',
                                                                                 'Restrictive CCS Development',
                                                                                 'Double ZCF'))
lsecost <- lsecost %>% pivot_longer(c('EnergyPayment',	'TransLossCost',	'CapacityPayment',
                                      'RPS',	'RGGI Revenue',	'PJM_CO2Cap',	'NSE',	'Subsidy_Cost',	'CongestionRent',	'TransCost')) %>%
  rename(`CostType`=name, `2020USD`= value, Year = year) %>%
  mutate(`2020USD/MWh`=`2020USD`/`TotalLoad`/1e6)
lsecost$`CostType` <- factor(lsecost$`CostType`,levels = c('RPS',	'RGGI Revenue',	'PJM_CO2Cap',	'NSE',	'Subsidy_Cost', 
                                                           'EnergyPayment',	'TransLossCost',	'CapacityPayment',	'CongestionRent',	'TransCost'))
lsecost$Scenario <- factor(lsecost$Scenario, levels = c('Current Policy','Stated Policy','Deep Decarbonization'));
lsecost_total <- lsecost %>% group_by(Scenario,`Tech Sensitivity`,Year) %>% summarize(`2020USD/MWh` = sum(`2020USD/MWh`));
ggplot(data = lsecost, aes(x=`Year`,y = `2020USD/MWh`))+
  geom_col(aes(fill = `CostType`))+
  scale_fill_manual(values = c('#fccb6e',
                               '#92CF72',
                               '#33A02C',
                               '#408DBF',
                               '#FFDAB9',
                               '#E94330',
                               '#DDA0DD',
                               '#B294C7',
                               '#6A3D9A',
                               '#2F4F4F',
                               '#696969',
                               '#cd51e0'))+
  geom_line(data=regioncost_total, aes(x=`Year`,y = `2020USD/MWh`)) + 
  geom_point(data=regioncost_total, aes(x=`Year`,y = `2020USD/MWh`)) + 
  facet_grid(Scenario~`Tech Sensitivity`) + 
  ggsave(paste0(RunFdr,'/Graphics/LSECostBreakDown.png'),width = 20,height=10)
