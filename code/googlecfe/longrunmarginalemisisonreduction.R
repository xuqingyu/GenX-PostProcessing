


system_emission <- read_csv(paste0(temp_RunFdr,"/CompiledResults/system_emissions_hourly.csv")) %>%
  mutate(HourlyEmission = `Local and Import Emission Rate`*`Total Withdraw`) %>%
  select(Scenario, TechSensitivity, Time_index,HourlyEmission)
reference = system_emission %>%
  filter(grepl("No 24x7 Purchase", TechSensitivity)) %>%
  rename(Ref_Emission = HourlyEmission) %>%
  select(Scenario, Time_index, Ref_Emission)
system_emission_reduction = left_join(system_emission, reference) %>%
  mutate(EmissionReduction = Ref_Emission - HourlyEmission) %>%
  filter(!grepl("No 24x7 Purchase", TechSensitivity))

cfe_timeseries <- read_csv(paste0(temp_RunFdr,"/CompiledResults/CFE_timeseries.csv"))
cfe_timeseries_cfe <- cfe_timeseries %>%
  filter(name == 'CFE') %>%
  filter(!grepl("No 24x7 Purchase", TechSensitivity))
cfe_timeseries_storagedispatch <- cfe_timeseries %>%
  filter(name == 'Storage Discharge') %>%
  filter(!grepl("No 24x7 Purchase", TechSensitivity)) %>%
  rename(discharge = name) %>%
  rename(discharge_value = value)

longrunmarginalemisisonreduction = left_join(cfe_timeseries_cfe,system_emission_reduction) %>%
  left_join(cfe_timeseries_storagedispatch) %>%
  mutate(lrmer = EmissionReduction/(value+discharge_value))
longrunmarginalemisisonreduction$lrmer[which(longrunmarginalemisisonreduction$value<50)] <- NA
temp = longrunmarginalemisisonreduction %>%
  filter(grepl("^10%",Scenario),
         grepl("Hourly 100%", TechSensitivity),
         !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
  group_by(Scenario,TechSensitivity)%>%
  mutate(normalizedlrmer = (lrmer-min(lrmer,na.rm = T))/(max(lrmer,na.rm = T)-min(lrmer,na.rm = T)))

tempload_load <- cfe_timeseries %>%
  filter(name == 'Load') %>%
  filter(grepl("^10%",Scenario),
         grepl("Hourly 100%", TechSensitivity),
         !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
  group_by(Scenario,TechSensitivity)%>%
  mutate(normalizedload = (value-min(value))/(max(value)-min(value)))
tempload_solar <- cfe_timeseries %>%
  filter(name == 'Utility Solar') %>%
  filter(grepl("^10%",Scenario),
         grepl("Hourly 100%", TechSensitivity),
         !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
  group_by(Scenario,TechSensitivity)%>%
  mutate(normalizedsolar = (value-min(value))/(max(value)-min(value)))

period = 1
ggplot()+
  geom_line(data = temp, aes(x = Time_index, y = normalizedlrmer),color = 'red')+
  geom_line(data = tempload_load, aes(x = Time_index, y = normalizedload),color = 'green')+
  geom_line(data = tempload_solar, aes(x = Time_index, y = normalizedsolar),color = 'blue')+
  coord_cartesian(ylim = c(0,1), xlim = c(1+(period - 1)*168,(period)*168)) +
  scale_x_continuous(breaks = seq(1+(period - 1)*168,(period)*168, 12))+
  facet_grid(Scenario~.)

temp = longrunmarginalemisisonreduction %>%
  filter(grepl("^10%",Scenario),
         grepl("Hourly 80%", TechSensitivity),
         !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
  group_by(Scenario,TechSensitivity)%>%
  mutate(normalizedlrmer = (lrmer-min(lrmer,na.rm = T))/(max(lrmer,na.rm = T)-min(lrmer,na.rm = T)))

tempload_load <- cfe_timeseries %>%
  filter(name == 'Load') %>%
  filter(grepl("^10%",Scenario),
         grepl("Hourly 80%", TechSensitivity),
         !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
  group_by(Scenario,TechSensitivity)%>%
  mutate(normalizedload = (value-min(value))/(max(value)-min(value)))
tempload_solar <- cfe_timeseries %>%
  filter(name == 'Utility Solar') %>%
  filter(grepl("^10%",Scenario),
         grepl("Hourly 80%", TechSensitivity),
         !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
  group_by(Scenario,TechSensitivity)%>%
  mutate(normalizedsolar = (value-min(value))/(max(value)-min(value)))

period = 1
ggplot()+
  geom_line(data = temp, aes(x = Time_index, y = normalizedlrmer),color = 'red')+
  geom_line(data = tempload_load, aes(x = Time_index, y = normalizedload),color = 'green')+
  geom_line(data = tempload_solar, aes(x = Time_index, y = normalizedsolar),color = 'blue')+
  coord_cartesian(ylim = c(0,1), xlim = c(1+(period - 1)*168,(period)*168)) +
  scale_x_continuous(breaks = seq(1+(period - 1)*168,(period)*168, 12))+
  facet_grid(Scenario~.)


temp = longrunmarginalemisisonreduction %>%
  filter(grepl("^10%",Scenario),
         !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
  group_by(Scenario,TechSensitivity)%>%
  mutate(miner = min(lrmer,na.rm = T),maxer = max(lrmer,na.rm = T)) %>%
  mutate(normalizedlrmer = (lrmer-miner)/(maxer-miner)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list))

period = 3
ggplot()+
  geom_line(data = filter(temp), aes(x = Time_index, y = lrmer, color = TechSensitivity)) +
  geom_line(data = tempload_load, aes(x = Time_index, y = normalizedload),color = 'green')+
  geom_line(data = tempload_solar, aes(x = Time_index, y = normalizedsolar),color = 'blue')+
  coord_cartesian(ylim = c(0,2),xlim = c(1+(period - 1)*168,(period)*168)) +
  scale_x_continuous(breaks = seq(1+(period - 1)*168,(period)*168, 12))+
  facet_grid(Scenario~.) +
  scale_color_brewer(palette = 'Set3')+
  theme_bw()

