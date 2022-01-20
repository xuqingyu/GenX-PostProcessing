
# source('./code/Header.R')
# Calculate subregion cost
# Modified on Aug 27, 2021: this work in general
gen_profit <- read_csv(paste0(RunFdr,'/CompiledResults/Settlement_short.csv'),
                       col_types = cols()) %>%
  left_join(resource_mapping) %>%
  filter(EndCap>20) %>%
  na.omit() %>%
  mutate(`Tech Subsidy Revenue` = SubsidyRevenue + RegSubsidyRevenue) %>%
  rename(`Energy Revenue` = EnergyRevenue,
         `Capacity Revenue` = ReserveMarginRevenue,
         `RPS Revenue` = `RPSRevenue`,
         `Fuel and VOM` = VOM_n_Fuel,
         `CAPEX` = `Inv_cost`,
         `Energy Charge Payment` = `Charge_cost`,
         `Emission Cost` = EmissionsCost,
         `Emission Capture Cost` = EmissionsCapture,
         `Sunk Cost` = SunkCost) %>%
  select(-c(SubsidyRevenue,RegSubsidyRevenue))
  
for ( i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_profit_subregion <- gen_profit %>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel) %>%
    summarize(AnnualOutput = sum(Sum),
              AnnualCharge = sum(ChargeSum),
              Capacity = sum(EndCap),
              `Energy Capacity` = sum(EndEnergyCap),
              `Energy Revenue` = sum(`Energy Revenue`),
              `Energy Charge Payment` = sum(`Energy Charge Payment`),
              `Capacity Revenue` = sum(`Capacity Revenue`),
              `RPS Revenue` = sum(`RPS Revenue`),
              `Fuel and VOM` = sum(`Fuel and VOM`),
              `FOM` = sum(`FOM`),
              `CAPEX` = sum(`CAPEX`),
              `Sunk Cost` = sum(`Sunk Cost`),
              `Emission Cost` = sum(`Emission Cost`),
              `Emission Capture Cost` = sum(`Emission Capture Cost`),
              `Tech Subsidy Revenue` = sum(`Tech Subsidy Revenue`)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Fuel, Scenario, `TechSensitivity`,
           AnnualOutput, AnnualCharge, Capacity,`Energy Capacity`,
           `Energy Revenue`, `Energy Charge Payment`,`Capacity Revenue`,`RPS Revenue`,
           `Fuel and VOM`,`FOM`,`CAPEX`,`Sunk Cost`, `Emission Cost`, `Emission Capture Cost`,
           `Tech Subsidy Revenue`)
  write_csv(gen_profit_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Profit_',temp_total_title,".csv"))
}







