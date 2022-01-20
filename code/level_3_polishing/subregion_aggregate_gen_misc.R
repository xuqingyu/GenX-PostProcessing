# Subsidy ----
# Modified on Sept 1, 2021
# Modification: remove the calculation for storage because it does not make sense to have a 
# per MWh output measurement for storage facilities.
# Modification 2: some reformatting
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_profit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                    '/Generation/Gen_Profit_',temp_total_title,".csv")
  if (file.exists(gen_profit_subregion_fn)){
    subsidy_subregion <- read_csv(gen_profit_subregion_fn, col_types = cols()) %>%
      filter(!(Fuel %in% storage_fuel))  %>%
      select(case, year, Fuel, Scenario, TechSensitivity, AnnualOutput, Capacity , 
             `Tech Subsidy Revenue`) %>%
      mutate(`Tech Subsidy Revenue per MWh` = `Tech Subsidy Revenue`/AnnualOutput);
  }
  write_csv(subsidy_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                     '/Generation/Gen_Subsidy_',temp_total_title,".csv"))
}
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_profit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                    '/Generation/Gen_Profit_',temp_total_title,".csv")
  if (file.exists(gen_profit_subregion_fn)){
    subsidy_includingRPS_subregion <- read_csv(gen_profit_subregion_fn, 
                                               col_types = cols()) %>%
      filter(!(Fuel %in% storage_fuel))  %>%
      select(case, year, Fuel, Scenario, TechSensitivity, AnnualOutput,Capacity , 
             `Tech Subsidy Revenue`, `RPS Revenue`) %>%
      mutate(`Subsidy (with RPS) Revenue per MWh` = round((`Tech Subsidy Revenue` + `RPS Revenue`)/AnnualOutput,digits=2));
  }
  write_csv(subsidy_includingRPS_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                                  '/Generation/Gen_Subsidy_wRPS_',temp_total_title,".csv"))
}

for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_profit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                    '/Generation/Gen_Profit_',temp_total_title,".csv")
  if (file.exists(gen_profit_subregion_fn)){
    captured_energy_price_subregion <- read_csv(gen_profit_subregion_fn, col_types = cols()) %>%
      filter(!(Fuel %in% storage_fuel))  %>%
      select(case, year, Fuel, Scenario, TechSensitivity, AnnualOutput, Capacity , 
             `Energy Revenue`) %>%
      mutate(`Energy Revenue per MWh` = `Energy Revenue`/AnnualOutput) ;
  }
  write_csv(captured_energy_price_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                                   '/Generation/Gen_CapturedEnergyPrice_',temp_total_title,".csv"))
}

for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_profit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                    '/Generation/Gen_Profit_',temp_total_title,".csv")
  if (file.exists(gen_profit_subregion_fn)){
    capacity_factor_subregion <- read_csv(gen_profit_subregion_fn, col_types = cols()) %>%
      filter(!(Fuel %in% storage_fuel)) %>%
      select(case, year, Fuel, Scenario, TechSensitivity, AnnualOutput, Capacity) %>%
      mutate(`Capacity Factor` = AnnualOutput/Capacity/8760);
    capacity_factor_subregion$`Capacity Factor`[is.infinite(capacity_factor_subregion$`Capacity Factor`)] <- 'No Capacity'
  }
  write_csv(capacity_factor_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                             '/Generation/Gen_CapacityFactor_',temp_total_title,".csv"))
}

for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_profit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                    '/Generation/Gen_Profit_',temp_total_title,".csv")
  if (file.exists(gen_profit_subregion_fn)){
    LCOE_subregion <- read_csv(gen_profit_subregion_fn, col_types = cols()) %>%
      filter(!(Fuel %in% storage_fuel), AnnualOutput > 1000) %>%
      select(case, year, Fuel, Scenario, TechSensitivity, AnnualOutput, 
             `Fuel and VOM`,	FOM,	CAPEX,	`Sunk Cost`,	`Emission Cost`,	`Emission Capture Cost`) %>%
      mutate(`LCOE` = (-1)*(`Fuel and VOM`+	FOM + CAPEX + `Sunk Cost`+ `Emission Cost` +	`Emission Capture Cost`)/AnnualOutput);
  }
  write_csv(LCOE_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                  '/Generation/Gen_LCOE_',temp_total_title,".csv"))
}
