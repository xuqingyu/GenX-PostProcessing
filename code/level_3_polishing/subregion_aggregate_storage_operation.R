
stor_power <- read_csv(paste0(RunFdr,'CompiledResults/power.csv'), 
                      col_types = cols()) %>%
  left_join(resource_mapping) %>%
  filter((Fuel %in% storage_fuel)) %>%
  na.omit()
stor_charge <- read_csv(paste0(RunFdr,'CompiledResults/charge.csv'), 
                      col_types = cols()) %>%
  left_join(resource_mapping) %>%
  filter((Fuel %in% storage_fuel)) %>%
  na.omit()
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  storoutput_subregion <- stor_power %>%
    filter(Region %in% temp_total) %>%
    group_by(case, year, Fuel) %>%
    summarize(AnnualOutput = sum(AnnualSum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualOutput)
  storcharge_subregion <- stor_charge %>%
    filter(Region %in% temp_total) %>%
    group_by(case, year, Fuel) %>%
    summarize(AnnualCharge = sum(AnnualSum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualCharge)
  stor <- storoutput_subregion %>%
    left_join(storcharge_subregion) %>%
    mutate(AnnualLoss = AnnualCharge - AnnualOutput)
  write_csv(stor,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                       '/Generation/Stor_Operation_',temp_total_title,".csv"))
  rm(storoutput_subregion,storcharge_subregion,stor)
}
rm(stor_power, stor_charge)
