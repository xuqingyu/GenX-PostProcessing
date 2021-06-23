# Cost vs Emissions ----
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  # reading total LSE cost
  lse_payment_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_',temp_total_title,"_with2019_and_DG.csv")
  lse_payment_total <- read_csv(lse_payment_subregion_fn) %>%
    mutate(`LSE Net Payment` = `Energy Payment` + `Transmission Loss Cost` + `NSE Cost`+ `Capacity Payment`+ `CO2 Revenue Mass Cap` +
             `CO2 Revenue Load Rate Cap` + `RPS Total Payment` + `Tech Subsidy Cost` + `Transmission Cost` + `Congestion Revenue`) %>%
    mutate(`LSE Net Payment (with DG)` = `LSE Net Payment` + `NJ DG Cost`) %>% 
    mutate(`LSE Net Payment ($/MWh)` = `LSE Net Payment`/`Gross Total`,
           `LSE Net Payment (with DG, $/MWh)` = `LSE Net Payment (with DG)`/`Gross Total`) %>%
    select(case, year, Scenario, TechSensitivity, AnnualLoad, `Gross Total`,
           `LSE Net Payment`, `LSE Net Payment (with DG)`,
           `LSE Net Payment ($/MWh)`,`LSE Net Payment (with DG, $/MWh)`)
  # reading system cost
  system_cost_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/System_Cost_',temp_total_title,".csv")
  system_cost_total <- read_csv(system_cost_subregion_fn) %>%
    mutate(`System Cost` = `Energy Import Cost` + `Energy Export Revenue` + `NSE Cost` +
             `Fuel and VOM` + `FOM` + `Sunk Cost` + CAPEX + `Emission Capture Cost`+ 
             `Transmission Cost` + `Capacity Import Cost`) %>%
    mutate(`System Cost (with DG)` = `System Cost` + `NJ DG Cost`) %>% 
    mutate(`System Cost ($/MWh)` = `System Cost`/`Gross Total`,
           `System Cost (with DG, $/MWh)` = `System Cost (with DG)`/`Gross Total`) %>%
    select(case, year, Scenario, TechSensitivity, AnnualLoad, `Gross Total`,
           `System Cost`, `System Cost (with DG)`,
           `System Cost ($/MWh)`,`System Cost (with DG, $/MWh)`)
  
  emissions_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Emissions/Emissions_',temp_total_title,"_with2019.csv")
  emissions_total <- read_csv(emissions_subregion_fn)
  
  lse_cost_vs_emission <- left_join(lse_payment_total,emissions_total)
  system_cost_vs_emission <- left_join(system_cost_total,emissions_total)
  write_csv(lse_cost_vs_emission,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/LSE_Cost_Emission_Tradeoff',temp_total_title,".csv"))
  write_csv(system_cost_vs_emission,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/System_Cost_Emission_Tradeoff',temp_total_title,".csv"))
}


