source('./code/Header.R')
systemview_fn <- paste0(RunFdr, "/CompiledResults/SystemView_CE_temp.csv");
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
                                                                                 'Double ZCF',
                                                                                 'No New Gas'))
for (y in unique(SystemView$year)) {
  tempdata <- SystemView %>% filter(year ==y) 
  ggplot(data = tempdata, aes(x=`Load Emission Rate (Metric ton/MWh)`,y = `System Cost ($/MWh)`,color = Scenario,size= 6))+
    geom_point(aes(shape = `Tech Sensitivity`)) +
    scale_shape_manual(values=c(2,15,0,16,21,3,4,5,6)) + 
    scale_x_reverse()+
    xlim(c(0,0.7))+
    ylim(c(30,110))+
    ggsave(paste0(RunFdr,'/Graphics/PJM System Emission Trade-Off in Year ',y,'.png'),width = 10,height=10)
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
