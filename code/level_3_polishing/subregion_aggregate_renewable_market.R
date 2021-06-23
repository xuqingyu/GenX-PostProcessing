# Calculate policy target ----
# source('./code/Header.R')
lse_rpsload <- read_csv(paste0(RunFdr,'/CompiledResults/LSERPSElgibileLoad.csv')) %>%
  rename(zone = Zone);
lse_rpsload$zone <- as.factor(lse_rpsload$zone);
lse_rpsload <- left_join(lse_rpsload, zone_mapping) %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
rps_constraint <- read_csv(paste0(RunFdr,'/CompiledResults/RPS_constraints.csv')) %>%
  left_join(cases_newnames, by = c('case' = 'case_description')) %>%
  mutate(zone = str_remove(`Network_zones`,'z')) %>%
  rename(region = `Region description`) %>%
  select(case,year, `RPS_Constraint_Name`,region,zone,value)
rps_constraint_target <- left_join(rps_constraint, lse_rpsload) %>%
  mutate(target = value * `RPSElgibilableLoad_MWh`/1e6) %>%
  select(case,year, `RPS_Constraint_Name`,Scenario, TechSensitivity, region,zone,target)

rps_price <- read_csv(paste0(RunFdr,'/CompiledResults/RPS_CES.csv')) %>%
  mutate(RPS_Constraint_Name = paste('RPS_',ID,sep=""),
         RPS_Price = round(RPS_Price,digits=3)) %>%
  left_join(cases_newnames, by = c('case' = 'case_description')) %>%
  select(case,year,`RPS_Constraint_Name`,Scenario, TechSensitivity, RPS_Price) %>%
  write_csv(paste0(RunFdr,'/CompiledResults/RPS_prices.csv'))
  
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_output_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Output_',temp_total_title,".csv")

  temp_re_output <- read_csv(gen_output_fn) %>%
    filter(Fuel %in% renewable_fuel) %>%
    group_by(case,year,Scenario,TechSensitivity) %>%
    summarise(TotalREOutput = sum(AnnualOutput)/1e6) # into TWh
  temp_ces_output <- read_csv(gen_output_fn) %>%
    filter(Fuel %in% clean_fuel) %>%
    group_by(case,year,Scenario,TechSensitivity) %>%
    summarise(TotalCEOutput = sum(AnnualOutput)/1e6) # into TWh
  temp_rece_output <- left_join(temp_re_output, temp_ces_output)
  write_csv(temp_rece_output, paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Clean_Output_',temp_total_title,".csv"))
  
  temp_rps_constraint_target <- rps_constraint_target %>%
    filter(region %in% temp_total) %>%
    group_by(case,year, `RPS_Constraint_Name`,Scenario, TechSensitivity) %>%
    summarise(target = sum(target))
  write_csv(temp_rps_constraint_target, paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/RPS_Target_TWh_',temp_total_title,".csv"))
}
