settingfile <- 'postprocessing_inputs.csv';
RunFdr <-"/Users/qingyuxu/Documents/pjm_ce_all/"
source('./code/Header.R')
cap_price <- read_csv(paste0(RunFdr,'/CompiledResults/Capacity_Price.csv')) %>%
  filter(year == 2030, name == 'CapRes_1')

lse_cost_vs_emission <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                        Subregions[i],'/LSE_Cost_Emission_TradeoffPJM.csv')) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))

cleanenergy <- read_csv(paste0(RunFdr,'/CompiledResults/',
                               Subregions[i],'/Generation/Gen_Output_PJM.csv')) %>%
  filter(Fuel %in% na.omit(clean_fuel)) %>%
  group_by(year, Scenario, TechSensitivity) %>%
  summarize(TotalCEOutput = sum(AnnualOutput)/1e6) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity))

storage_loss <- read_csv(paste0(RunFdr,'/CompiledResults/',
                                Subregions[i],'/Generation/Stor_Operation_',
                                temp_total_title,".csv")) %>%
  group_by(year, Scenario, TechSensitivity) %>%
  summarize(AnnualLoss = sum(AnnualLoss)) %>%
  select(year, Scenario, TechSensitivity, AnnualLoss)

lse_clean_energy_share = left_join(lse_cost_vs_emission, cleanenergy) %>%
  left_join(storage_loss) %>%
  mutate(CEshare = round(TotalCEOutput*1e6/(`Gross Total` + AnnualLoss),2))

cap_price <- left_join(cap_price, lse_clean_energy_share)

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
  "No Nuclear Retirement" = '#b15928')
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
  "No Nuclear Retirement" = 2)

MajorTechSensitivity <- c('Mid','Low RE/BESS Cost', 'Low NatGas Price', 
                          'High RE/BESS Cost','High NatGas Price')
Policy = rep('No Federal Policy',nrow(cap_price))
Policy[grep('Clean Energy Standard',cap_price$Scenario)] = 'Clean Energy Standard';
Policy[grep('Cap-and-Trade',cap_price$Scenario)] = 'Carbon Cap-and-Trade';
cap_price_w_policy <- cbind(cap_price, Policy) %>%
  filter(year == '2030',
         TechSensitivity %in% MajorTechSensitivity,
         !(Scenario %in% ScenarioFilter)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = MajorTechSensitivity)) %>%
  filter(Policy %in%  c('Clean Energy Standard', 'Carbon Cap-and-Trade'))

for (w in c(c('Clean Energy Standard','Carbon Cap-and-Trade'))){
  if (w == 'Clean Energy Standard') {
    xlabel = '% of Total Load Supported by Clean Energy '
    ggplot()+
      geom_point(data = filter(cap_price_w_policy,
                               Policy == w,
                               TechSensitivity == 'Mid'),
                 aes(x = CEshare,
                     y = round(`Capacity Price`,2),
                     color = TechSensitivity),
                 size = 3)+
      geom_point(data = filter(cap_price_w_policy,
                               Policy == w,
                               TechSensitivity != 'Mid'),
                 aes(x = CEshare,
                     y = round(`Capacity Price`,2),
                     color = TechSensitivity))+
      geom_line(data = filter(cap_price_w_policy,
                              Policy == w),
                aes(x = CEshare,
                    y = round(`Capacity Price`,2),
                    color = TechSensitivity))+
      scale_color_manual(values = TechSensitivityColorCode)+
      scale_x_continuous(limits = c(0.2,1), labels =scales::percent, breaks = seq(0.2,1,0.1))+
      xlab("% of Total Load Supported by Clean Energy")+
      theme_classic2() +
      theme(legend.position = c(0.2,0.8),
            legend.key = element_rect(fill = "white", colour = "black"),
            legend.title = element_blank(),
            axis.line = element_line(colour = "black",size=0),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour = "black", size=1.5))+
      coord_cartesian(ylim = c(20,70))+
      scale_y_continuous(breaks = seq(20,70,5))+
      ylab("Capacity Price ($/MW-day)") +
      guides(color = guide_legend(ncol = 1, title.position = "top"))+
      ggsave(paste0(RunFdr,'/Graphics/Capacity price_',w,'.png'),
             width = 7,
             height= 7)      
  } else {
    xlabel = '% Emission Reduction Compared to the 2005 Level'
    ggplot()+
      geom_point(data = filter(cap_price_w_policy,
                               Policy == w,
                               TechSensitivity == 'Mid'),
                 aes(x = `Load Emissions Rate (Ton/MWh)`,
                     y = round(`Capacity Price`,2),
                     color = TechSensitivity),
                 size = 3)+
      geom_point(data = filter(cap_price_w_policy,
                               Policy == w,
                               TechSensitivity != 'Mid'),
                 aes(x = `Load Emissions Rate (Ton/MWh)`,
                     y = round(`Capacity Price`,2),
                     color = TechSensitivity))+
      geom_line(data = filter(cap_price_w_policy,
                               Policy == w),
                 aes(x = `Load Emissions Rate (Ton/MWh)`,
                     y = round(`Capacity Price`,2),
                     color = TechSensitivity))+
      scale_color_manual(values = TechSensitivityColorCode)+
      coord_cartesian(ylim = c(20,70))+
      scale_y_continuous(breaks = seq(20,70,5))+
      scale_x_reverse(limits = c(0.5,0),
                      sec.axis = sec_axis(~ (1-./0.607), labels = scales::percent,
                                          name = '% reduction from 2005 emissions level = 0.607 ton/MWh',
                                          breaks = seq(from = 0, to = 1, by = 0.1)))+
      theme_classic2() +
      ylab("Capacity Price ($/MW-day)") +
      xlab("Load Emission Rate (ton/MWh)")+
      theme(legend.position = c(0.2,0.8),
            legend.key = element_rect(fill = "white", colour = "black"),
            legend.title = element_blank(),
            axis.line = element_line(colour = "black",size=0),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour = "black", size=1.5))+
      guides(color = guide_legend(ncol = 1, title.position = "top"))+
      ggsave(paste0(RunFdr,'/Graphics/Capacity price_',w,'.png'),
             width = 7,
             height= 7)
  }
  

}
