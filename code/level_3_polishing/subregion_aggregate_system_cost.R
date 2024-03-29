# Calculate system cost of each subregion ----
# source('./code/Header.R')
CarbonStorageRevenue_fn <- paste0(RunFdr,'/CompiledResults/LSECO2Revenue_loadrate.csv')
if (file.exists(CarbonStorageRevenue_fn)) {
  CarbonStorageRevenue <- read_csv(CarbonStorageRevenue_fn, 
                                   col_types = cols()) %>%
    filter(grepl('StorageLoss', item))
}
ESRStoragePayment_fn <- paste0(RunFdr,'/CompiledResults/ESR_PaymentStorageloss.csv')
if (file.exists(ESRStoragePayment_fn)) {
  ESRStoragePayment <- read_csv(ESRStoragePayment_fn, 
                                col_types = cols())
}
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_payment_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_',temp_total_title,"_with2019_and_DG.csv")
  gen_profit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Profit_',temp_total_title,".csv")
  if (file.exists(lse_payment_subregion_fn)){
    lse_payment_plot <- read_csv(lse_payment_subregion_fn, col_types = cols())
  }
  if (exists('CarbonStorageRevenue')) {
    CarbonStorageRevenue_subregion = CarbonStorageRevenue %>%
      mutate(Zone = factor(Zone, levels = zone_mapping$zone)) %>%
      left_join(zone_mapping, by = c('Zone' = 'zone')) %>%
      filter(region %in% temp_total) %>%
      group_by(case,year)  %>%
      summarize(`Emission Revenue Storage` = -1 * sum(value)) %>%
      left_join(cases_newnames, by = c('case' = 'case_description'))
  }
  if (exists('ESRStoragePayment')) {
    ESRStoragePayment_subregion = ESRStoragePayment %>%
      mutate(Zone = factor(Zone, levels = zone_mapping$zone)) %>%
      left_join(zone_mapping, by = c('Zone' = 'zone')) %>%
      filter(region %in% temp_total) %>%
      group_by(case,year)  %>%
      summarize(`ESR Payment Storage` = sum(value)) %>%
      left_join(cases_newnames, by = c('case' = 'case_description'))
  }
  if (file.exists(gen_profit_subregion_fn)){
    gen_profit_subregion_total <- read_csv(gen_profit_subregion_fn, col_types = cols()) %>%
      group_by(case,year,Scenario, `TechSensitivity`) %>%
      summarize(`Energy Revenue` = (-1) * sum(`Energy Revenue`),
                `Energy Charge Payment` = (-1) * sum(`Energy Charge Payment`),
                `Capacity Revenue` = (-1) * sum(`Capacity Revenue`),
                `ESR Revenue` = (-1) * sum(`ESR Revenue`),
                `Fuel and VOM` = (-1) * sum(`Fuel and VOM`),
                `FOM` = (-1) * sum(`FOM`),
                `CAPEX` = (-1) * sum(`CAPEX`),
                `Sunk Cost` = (-1) * sum(`Sunk Cost`),
                `Emission Cost` = (-1) * sum(`Emission Cost`),
                `Emission Capture Cost` = (-1) * sum(`Emission Capture Cost`),
                `Tech Subsidy Revenue` = (-1) * sum(`Tech Subsidy Revenue`));
  }
  system_cost <- left_join(lse_payment_plot, gen_profit_subregion_total, 
                           by = c('case', 'year','Scenario','TechSensitivity'));
  if (exists('CarbonStorageRevenue_subregion')) {
    system_cost <- left_join(system_cost, CarbonStorageRevenue_subregion,
                             by = c('case', 'year','Scenario','TechSensitivity')) %>%
      mutate(`CO2 Revenue` = `CO2 Revenue` + natozero(`Emission Revenue Storage`)) %>%
      select(-`Emission Revenue Storage`)
  }
  if (exists('ESRStoragePayment')) {
    system_cost <- left_join(system_cost, ESRStoragePayment_subregion,
                             by = c('case', 'year','Scenario','TechSensitivity')) %>%
      mutate(`RPS Total Payment` = `RPS Total Payment` + natozero(`ESR Payment Storage`))%>%
      select(-`ESR Payment Storage`)
  }
  system_cost <- system_cost %>%
    group_by(case,year,Scenario, `TechSensitivity`) %>%
    mutate(`Energy Import Cost` = max(0, `Energy Payment` + `Congestion Revenue` + `Transmission Loss Cost` + `Energy Revenue` + `Energy Charge Payment`),
           `Energy Export Revenue` = min(0, `Energy Payment` + `Congestion Revenue` + `Transmission Loss Cost` + `Energy Revenue` + `Energy Charge Payment`),
           `Capacity Import Cost` = max(0, `Capacity Payment` + `Capacity Revenue`),
           `Capacity Export Revenue` = min(0, `Capacity Payment` + `Capacity Revenue`),
           `RPS Import Cost` = max(0, `RPS Total Payment` + `ESR Revenue`),
           `RPS Export Revenue` = min(0, `RPS Total Payment` + `ESR Revenue`),
           `CO2 Import Cost` = max(0, `Emission Cost` + `CO2 Revenue`),
           `CO2 Export Revenue` = min(0, `Emission Cost` + `CO2 Revenue`)) %>%
    select(-c(`Energy Payment`,`Congestion Revenue`,`Transmission Loss Cost`,
              `Energy Revenue`,`Energy Charge Payment`,
              `Capacity Payment`, `Capacity Revenue`,
              `Tech Subsidy Cost`, `Tech Subsidy Revenue`,
              `ESR Revenue`, `RPS Total Payment`,
              `CO2 Revenue`,`Emission Cost`)) %>%
    filter(year != 2019);
  gross_load <- read_csv(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Load/Load_Component_",Subregions[i],".csv"), 
                         col_types = cols()) %>%
    filter(`Load Type` == 'Gross Total') %>%
    mutate(`Gross Total` = TWh*1e6) %>%
    select(-c(`Load Type`,TWh))
  system_cost[is.na(system_cost)] <- 0
  system_cost <- left_join(system_cost,gross_load)
  write_csv(system_cost,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/System_Cost_',temp_total_title,".csv"))
  system_cost_long <- pivot_longer(system_cost,cols= !c(case, year, Scenario, TechSensitivity, AnnualLoad, `Gross Total`),names_to = 'Cost Type') %>%
    mutate(`USD per MWh` = value/`Gross Total`)
  write_csv(system_cost_long,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/System_Cost_long_',temp_total_title,".csv"))
}


