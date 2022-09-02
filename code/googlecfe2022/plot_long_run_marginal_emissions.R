
# Step 1 calculate local load
selected_case = 'p2_94h_2030_currentpolicy_10ci_cip_hourly94'
selected_case_short = 'currentpolicy_10ci_cip_hourly94'
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
  pivot_longer(cols = c('Solar Profile', 'Modified Load (Scaled)', 'Long-Run Marginal Emission Rate'))

temp_ts_mapping_fn <- paste0(RunFdr,'/2030/',selected_case,"/Inputs/Period_map.csv");
temp_load_fn <- paste0(RunFdr,'/2030/',selected_case,'/Inputs/Load_data.csv')
ts_mapping <- read_csv(temp_ts_mapping_fn, col_types = cols())
n_slot <- dim(ts_mapping)[1]
Hours_per_period <- read_csv(temp_load_fn, col_types = cols())$Timesteps_per_Rep_Period %>% na.omit()
n_period <- read_csv(temp_load_fn,col_types = cols())$Rep_Periods %>% na.omit()
model_hours <- Hours_per_period*n_slot;
HourID = c(1:model_hours)
Slot <- rep(ts_mapping$Period_Index,each = Hours_per_period)
lrme_plot_wide <- pivot_wider(lrme_plot, names_from = 'name',values_from = value)
template <- as_tibble(cbind(HourID, Slot)) %>% 
  left_join(ts_mapping, by = c('Slot' = 'Period_Index')) %>% 
  mutate(Time_Index = rep(c(1:Hours_per_period),times = n_slot)) %>%
  mutate(Time_Index = Time_Index + Hours_per_period*(Rep_Period_Index-1))
lrme_plot_full <- left_join(template, lrme_plot_wide, by = c('Time_Index')) %>%
  select(HourID, case,year, Scenario, TechSensitivity, `Long-Run Marginal Emission Rate`,`Solar Profile`, `Modified Load (Scaled)`) %>%
  write_csv(paste0(RunFdr,'/CompiledResults/',subreg,'/longrunmarginalemissionrate.csv'))



# # Plot
# 
# if (Studyregion == 'WECC') {
#   period = 1
# } else {
#   period = 1
# }
# 
# range = c((1+(period - 1)*168): ((period)*168))
# hour_break = seq(1+(period - 1)*168,(period)*168, 12)
# 
# ggplot(data = filter(lrme_plot, Time_Index %in% range),
#        aes(x = Time_Index)) + 
#   geom_line(aes(y = value, color = name),linetype = 1) +
#   geom_hline(yintercept = 0, color = 'gray50') + 
#   scale_y_continuous(
#     limits = c(-0.5,1),
#     breaks = seq(-0.6,1,0.2),
#     labels = scales::label_number(accuracy = 0.01),
#     name = "Long-Run Marginal Emission Rate (ton/MWh)",
#     sec.axis = sec_axis( ~.*1, name="Scaled Profiles",
#                          labels = scales::label_number(accuracy = 0.01),
#                          breaks = seq(-0.6,1,0.2))
#   ) +
#   scale_x_continuous(breaks = hour_break)+
#   theme_classic2() +
#   labs(x = 'Hour') +
#   guides(color = guide_legend(nrow = 1))+
#   theme(legend.position = c(0.5, 0.12),
#         legend.background = element_blank(),
#         legend.box.background = element_rect(colour = "black"),
#         legend.title = element_blank()) +
#   ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/',Studyregion,'_Long_Run_Marginal_Emission_Rate.png'),
#          width = 8,
#          height = 4.5)

