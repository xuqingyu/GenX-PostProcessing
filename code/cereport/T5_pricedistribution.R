
settingfile <- 'sample_inputs_pjm_additional.csv';
RunFdr <- '/Users/qingyuxu/Documents/PJM_QX_2022_PH1_newwacc'
source('./code/Header.R')
energyprice_ts <- read_csv(paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries.csv')) %>%
  left_join(cases_newnames,by=c('case' = 'case_description')) %>%
  filter(Region  == 'PJM_WEST') %>%
  filter(year == 2030)

Policy = as.character(energyprice_ts$Scenario)
Policy[grep('Clean Energy Standard',energyprice_ts$Scenario)] = 'Clean Energy Standard';
Policy[grep('Cap-and-Trade',energyprice_ts$Scenario)] = 'Carbon Cap-and-Trade';

MajorTechSensitivity <- c('Mid',
                          'Low RE/BESS Cost', 
                          'Low NatGas Price', 
                          'High RE/BESS Cost',
                          'High NatGas Price',
                          "No Interregional Transmission Upgrade",
                          "No New Gas Installation", 
                          "Allow CCS Expansion", 
                          "New Gas Capacity Caped at 20% of Existing", 
                          "Half Interregional Transmission Upgrade", 
                          "No Nuclear Retirement")
ScenarioFilter = c('Cap-and-Trade (40% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (45% Reduction Compare to 2005 Level)',
                   'Cap-and-Trade (100% Reduction Compare to 2005 Level)',
                   'Clean Energy Standard (40%)',
                   'Clean Energy Standard (45%)',
                   'Clean Energy Standard (100%)')
for (k in 1:length(MajorTechSensitivity)) {
  energyprice_ts_w_policy = cbind(energyprice_ts, Policy) %>%
    filter(TechSensitivity == MajorTechSensitivity[k],
           Policy %in%  c('Clean Energy Standard',
                          'Carbon Cap-and-Trade')) %>%
    filter(!(Scenario %in% ScenarioFilter))
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (80% Reduction Compare to 2005 Level)'] = '80%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (90% Reduction Compare to 2005 Level)'] = '90%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (75% Reduction Compare to 2005 Level)'] = '75%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (70% Reduction Compare to 2005 Level)'] = '70%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (60% Reduction Compare to 2005 Level)'] = '60%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (50% Reduction Compare to 2005 Level)'] = '50%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (55% Reduction Compare to 2005 Level)'] = '55%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (65% Reduction Compare to 2005 Level)'] = '65%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (85% Reduction Compare to 2005 Level)'] = '85%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Cap-and-Trade (95% Reduction Compare to 2005 Level)'] = '95%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (65%)'] = '65%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (70%)'] = '70%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (75%)'] = '75%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (80%)'] = '80%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (85%)'] = '85%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (50%)'] = '50%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (55%)'] = '55%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (60%)'] = '60%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (90%)'] = '90%'
  energyprice_ts_w_policy$Scenario[energyprice_ts_w_policy$Scenario == 'Clean Energy Standard (95%)'] = '95%'
  
  for (w in c('Clean Energy Standard','Carbon Cap-and-Trade')){
    if (w == 'Clean Energy Standard') {
      xlabel = '% of Total Load Supported by Clean Energy '
    } else {
      xlabel = '% Emission Reduction Compared to the 2005 Level'
    }
    ggplot(data = filter(energyprice_ts_w_policy,
                         Policy == w),
           aes(x = Scenario, 
               y = Price))+
      geom_violin(color='black',
                  width = 0.7,
                  scale = 'area',
                  fill = "grey90") +
      geom_hline(yintercept = 0,size = 0.2,color = 'gray30')+
      stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red")+
      theme_classic2() +
      coord_cartesian(ylim = c(-100,400))+
      scale_y_continuous(breaks = seq(-100,400,50)) +
      ylab("Energy Price ($/MWh)") + 
      xlab(xlabel)+
      ggsave(paste0(RunFdr,'/Graphics/EnergyPrice/Energy Price of Region PJM_WEST ', w, '_',k,'.png'),
             width = 7,
             height= 4)

  }
  
}
