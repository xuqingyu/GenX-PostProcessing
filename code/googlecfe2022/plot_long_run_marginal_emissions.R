
# Step 1 calculate local load
selected_case = 'p2_92h_2030_currentpolicy_10ci_cip_hourly92'
selected_case_short = 'currentpolicy_10ci_cip_hourly92'
base_case = 'p2_ncp_2030_currentpolicy_10ci_nocip'

load <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Inputs/Load_data.csv'), 
                 col_types = cols())[,-c(1:9)]
load_base <- read_csv(paste0(RunFdr,'/2030/',base_case,'/Inputs/Load_data.csv'), 
                 col_types = cols())[,-c(1:9)]

nse <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Results/zonalnse.csv'), 
                col_types = cols())[-1,-1]
nse_base <- read_csv(paste0(RunFdr,'/2030/',base_case,'/Results/zonalnse.csv'), 
                     col_types = cols())[-1,-1]
generator <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Inputs/Generators_data.csv'), 
                      col_types = cols())
generator_base <- read_csv(paste0(RunFdr,'/2030/',base_case,'/Inputs/Generators_data.csv'), 
                      col_types = cols())
generator <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Inputs/Generators_data.csv'), 
                      col_types = cols())
generator_base <- read_csv(paste0(RunFdr,'/2030/',base_case,'/Inputs/Generators_data.csv'), 
                           col_types = cols())
power <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Results/power.csv'), 
                  col_types = cols())[-c(1,2),-1]
power_base <- read_csv(paste0(RunFdr,'/2030/',base_case,'/Results/power.csv'), 
                  col_types = cols())[-c(1,2),-1]
charge <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Results/charge.csv'), 
                  col_types = cols())[-c(1,2),-1]
charge_base <- read_csv(paste0(RunFdr,'/2030/',base_case,'/Results/charge.csv'), 
                       col_types = cols())[-c(1,2),-1]
gridsupplyemission <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Results/tfs_sf_and_local_emissionrate.csv'), 
                               col_types = cols())[,2]
gridsupplyemission_base <- read_csv(paste0(RunFdr,'/2030/',base_case,'/Results/tfs_sf_and_local_emissionrate.csv'), 
                               col_types = cols())[,2]
modifiedload  <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Results/tfs_modifiedload.csv'), 
                      col_types = cols())[-1,2]
modifiedload_base  <- read_csv(paste0(RunFdr,'/2030/',base_case,'/Inputs/RPSH_Load_data.csv'), 
                               col_types = cols())[,2]
plant_emissions  <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Results/emissions_plant.csv'), 
                               col_types = cols())[-c(1,2),-1]
shortfall <- read_csv(paste0(RunFdr,'/2030/',selected_case,'/Results/tfs_sf.csv'), 
                      col_types = cols())[-1,2]

if (Studyregion == 'WECC') {
  localzones = c(1:2)
} else {
  localzones = c(5:13)
}
load = rowSums(load[,localzones])
load_base = rowSums(load_base[,localzones])
nse = rowSums(nse[,localzones])
nse_base = rowSums(nse_base[,localzones])
drs <- which(generator$FLEX ==1 & generator$Zone %in% localzones)
drs_base <- which(generator_base$FLEX ==1 & generator_base$Zone %in% localzones)
stor <- which(generator$STOR >=1 & generator$Zone %in% localzones & generator$RPSH_1==1)
ccs <- which(generator$STOR ==0 & generator$FLEX == 0 & generator$Zone %in% localzones & generator$RPSH_1==1)

load_after_dr = load - nse + 
  rowSums(power[, drs]) - rowSums(charge[, drs]) + 
  rowSums(charge[, stor]) - rowSums(power[, stor]) - modifiedload + shortfall
demand_side_emissions = (load_after_dr) * gridsupplyemission + rowSums(plant_emissions[,ccs])

load_after_dr_base = load_base - nse_base + 
  rowSums(power_base[, drs_base]) - rowSums(charge_base[, drs_base]) 
demand_side_emissions_base = (load_after_dr_base) * gridsupplyemission_base

long_run_marginal_emission = (demand_side_emissions_base - demand_side_emissions) %>%
  mutate(Time_Index = c(1:nrow(demand_side_emissions_base)))
colnames(long_run_marginal_emission) <- c('LongRunMarginalEmission','Time_Index')
modifiedload_toplot <- modifiedload %>%
  mutate(Time_Index = c(1:nrow(modifiedload)))
colnames(modifiedload_toplot) <- c('ModifiedLoad','Time_Index')

cfe_ts = read_csv(paste0(RunFdr,"/CompiledResults/tfs_gents_table.csv")) %>%
  filter(case == selected_case_short)
cfe_ts_all = cfe_ts %>%
  filter(Fuel != 'Flexible Load' & !grepl('Storage', Fuel) & !grepl('Battery', Fuel))%>%
  group_by(Time_Index, Scenario,TechSensitivity) %>%
  summarize(MW = sum(MW)) %>%
  rename(totalcfe_MW = MW)

cfe_ts_solar = cfe_ts %>%
  filter(Fuel == 'Utility Solar') %>%
  rename(Solar_MW = MW)


lrme_plot = left_join(long_run_marginal_emission, modifiedload_toplot)%>%
  left_join(cfe_ts_all) %>%
  left_join(cfe_ts_solar) %>%
  mutate(`Long-Run Marginal Emission Rate` = LongRunMarginalEmission/totalcfe_MW) %>%
  mutate(`Solar Profile` = Solar_MW/max(Solar_MW),
         `Modified Load (Scaled)` = (ModifiedLoad-min(ModifiedLoad))/(max(ModifiedLoad)-min(ModifiedLoad))) %>%
  select(Scenario, TechSensitivity, Time_Index, `Long-Run Marginal Emission Rate`,`Solar Profile`, `Modified Load (Scaled)`,case,year) %>%
  pivot_longer(cols = c('Solar Profile', 'Modified Load (Scaled)'))
lrme_plot_wide <- pivot_wider(lrme_plot, names_from = 'name',values_from = value)
if (Studyregion == 'WECC') {
  period = 1
} else {
  period = 1
}

range = c((1+(period - 1)*168): ((period)*168))
hour_break = seq(1+(period - 1)*168,(period)*168, 12)
ggplot(data = filter(lrme_plot, Time_Index %in% range),
       aes(x = Time_Index)) + 
  geom_line(aes(y = round(`Long-Run Marginal Emission Rate`,3)), linetype = 1) +
  geom_line(aes(y = value, color = name),linetype = 1) +
  scale_y_continuous(
    limits = c(-0.5,1),
    breaks = seq(-0.6,1,0.2),
    labels = scales::label_number(accuracy = 0.01),
    name = "Long-Run Marginal Emission Rate (ton/MWh)",
    sec.axis = sec_axis( ~.*1, name="Scaled Profiles",
                         labels = scales::label_number(accuracy = 0.01),
                         breaks = seq(-0.6,1,0.2))
  ) +
  scale_x_continuous(breaks = hour_break)+
  # geom_hline(yintercept = 0, color='gray30')+
  theme_bw() +
  labs(x = 'Hour') +
  theme(legend.position = 'bottom',
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) +
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/Long_Run_Marginal_Emission_Rate.png'),
         width = 8,
         height = 4.5)
  

# system_emission <- read_csv(paste0(temp_RunFdr,"/CompiledResults/system_emissions_hourly.csv")) %>%
#   mutate(HourlyEmission = `Local and Import Emission Rate`*`Total Withdraw`) %>%
#   select(Scenario, TechSensitivity, Time_index,HourlyEmission)
# 
# reference = system_emission %>%
#   filter(grepl("No 24x7 Purchase", TechSensitivity)) %>%
#   rename(Ref_Emission = HourlyEmission) %>%
#   select(Scenario, Time_index, Ref_Emission)
# system_emission_reduction = left_join(system_emission, reference) %>%
#   mutate(EmissionReduction = Ref_Emission - HourlyEmission) %>%
#   filter(!grepl("No 24x7 Purchase", TechSensitivity))
# 
# cfe_timeseries <- read_csv(paste0(temp_RunFdr,"/CompiledResults/CFE_timeseries.csv"))
# cfe_timeseries_cfe <- cfe_timeseries %>%
#   filter(name == 'CFE') %>%
#   filter(!grepl("No 24x7 Purchase", TechSensitivity))
# cfe_timeseries_storagedispatch <- cfe_timeseries %>%
#   filter(name == 'Storage Discharge') %>%
#   filter(!grepl("No 24x7 Purchase", TechSensitivity)) %>%
#   rename(discharge = name) %>%
#   rename(discharge_value = value)
# 
# longrunmarginalemisisonreduction = left_join(cfe_timeseries_cfe,system_emission_reduction) %>%
#   left_join(cfe_timeseries_storagedispatch) %>%
#   mutate(lrmer = EmissionReduction/(value+discharge_value))
# longrunmarginalemisisonreduction$lrmer[which(longrunmarginalemisisonreduction$value<50)] <- NA
# temp = longrunmarginalemisisonreduction %>%
#   filter(grepl("^10%",Scenario),
#          grepl("Hourly 100%", TechSensitivity),
#          !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
#   group_by(Scenario,TechSensitivity)%>%
#   mutate(normalizedlrmer = (lrmer-min(lrmer,na.rm = T))/(max(lrmer,na.rm = T)-min(lrmer,na.rm = T)))
# 
# tempload_load <- cfe_timeseries %>%
#   filter(name == 'Load') %>%
#   filter(grepl("^10%",Scenario),
#          grepl("Hourly 100%", TechSensitivity),
#          !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
#   group_by(Scenario,TechSensitivity)%>%
#   mutate(normalizedload = (value-min(value))/(max(value)-min(value)))
# tempload_solar <- cfe_timeseries %>%
#   filter(name == 'Utility Solar') %>%
#   filter(grepl("^10%",Scenario),
#          grepl("Hourly 100%", TechSensitivity),
#          !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
#   group_by(Scenario,TechSensitivity)%>%
#   mutate(normalizedsolar = (value-min(value))/(max(value)-min(value)))
# 
# period = 1
# ggplot()+
#   geom_line(data = temp, aes(x = Time_index, y = normalizedlrmer),color = 'red')+
#   geom_line(data = tempload_load, aes(x = Time_index, y = normalizedload),color = 'green')+
#   geom_line(data = tempload_solar, aes(x = Time_index, y = normalizedsolar),color = 'blue')+
#   coord_cartesian(ylim = c(0,1), xlim = c(1+(period - 1)*168,(period)*168)) +
#   scale_x_continuous(breaks = seq(1+(period - 1)*168,(period)*168, 12))+
#   facet_grid(Scenario~.)
# 
# temp = longrunmarginalemisisonreduction %>%
#   filter(grepl("^10%",Scenario),
#          grepl("Hourly 80%", TechSensitivity),
#          !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
#   group_by(Scenario,TechSensitivity)%>%
#   mutate(normalizedlrmer = (lrmer-min(lrmer,na.rm = T))/(max(lrmer,na.rm = T)-min(lrmer,na.rm = T)))
# 
# tempload_load <- cfe_timeseries %>%
#   filter(name == 'Load') %>%
#   filter(grepl("^10%",Scenario),
#          grepl("Hourly 80%", TechSensitivity),
#          !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
#   group_by(Scenario,TechSensitivity)%>%
#   mutate(normalizedload = (value-min(value))/(max(value)-min(value)))
# tempload_solar <- cfe_timeseries %>%
#   filter(name == 'Utility Solar') %>%
#   filter(grepl("^10%",Scenario),
#          grepl("Hourly 80%", TechSensitivity),
#          !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
#   group_by(Scenario,TechSensitivity)%>%
#   mutate(normalizedsolar = (value-min(value))/(max(value)-min(value)))
# 
# period = 1
# ggplot()+
#   geom_line(data = temp, aes(x = Time_index, y = normalizedlrmer),color = 'red')+
#   geom_line(data = tempload_load, aes(x = Time_index, y = normalizedload),color = 'green')+
#   geom_line(data = tempload_solar, aes(x = Time_index, y = normalizedsolar),color = 'blue')+
#   coord_cartesian(ylim = c(0,1), xlim = c(1+(period - 1)*168,(period)*168)) +
#   scale_x_continuous(breaks = seq(1+(period - 1)*168,(period)*168, 12))+
#   facet_grid(Scenario~.)
# 
# 
# temp = longrunmarginalemisisonreduction %>%
#   filter(grepl("^10%",Scenario),
#          !grepl("Ex|CES|Hi.|45Q",Scenario),) %>%
#   group_by(Scenario,TechSensitivity)%>%
#   mutate(miner = min(lrmer,na.rm = T),maxer = max(lrmer,na.rm = T)) %>%
#   mutate(normalizedlrmer = (lrmer-miner)/(maxer-miner)) %>%
#   mutate(TechSensitivity = factor(TechSensitivity, levels = x_list))
# 
# period = 3
# ggplot()+
#   geom_line(data = filter(temp), aes(x = Time_index, y = lrmer, color = TechSensitivity)) +
#   geom_line(data = tempload_load, aes(x = Time_index, y = normalizedload),color = 'green')+
#   geom_line(data = tempload_solar, aes(x = Time_index, y = normalizedsolar),color = 'blue')+
#   coord_cartesian(ylim = c(0,2),xlim = c(1+(period - 1)*168,(period)*168)) +
#   scale_x_continuous(breaks = seq(1+(period - 1)*168,(period)*168, 12))+
#   facet_grid(Scenario~.) +
#   scale_color_brewer(palette = 'Set3')+
#   theme_bw()

