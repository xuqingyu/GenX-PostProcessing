# Capacity Plot ----
# source('./code/Header.R')
# Modified on Sept 1, 2021: make DG capacity more general
gen_capacity <- read_csv(paste0(RunFdr,'/CompiledResults/capacity.csv'), 
                         col_types = cols()) %>%
  left_join(resource_mapping) %>%
  filter(!(Fuel %in% flexiload_list)) %>%
  na.omit() %>%# filter out the "resource" that are not going to show
  mutate(Zone = as.factor(Zone)) %>%
  left_join(zone_mapping, by = c('Zone' = 'zone')) %>%
  rename(Region = region)

for ( i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  
  gen_capacity_subregion <- gen_capacity %>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel) %>%
    summarize( Capacity = sum(EndCap),
               `Charge Capacity` = sum(EndChargeCap),
               `Energy Capacity` = sum(EndEnergyCap),
               `Capacity Expansion` = sum(NewCap),
               `Energy Capacity Expansion` = sum(NewEnergyCap),
               `Charge Capacity Expansion` = sum(NewChargeCap),
               `Capacity Retirement` = sum(RetCap),
               `Energy Capacity Retirement` = sum(RetEnergyCap),
               `Charge Capacity Retirement` = sum(RetChargeCap)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case, year, Fuel, Scenario, TechSensitivity, 
           Capacity, `Charge Capacity`, `Energy Capacity`,
           `Capacity Expansion`, `Energy Capacity Expansion`, `Charge Capacity Expansion`, 
           `Capacity Retirement`, `Energy Capacity Retirement`, `Charge Capacity Retirement`)
  write_csv(gen_capacity_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                          '/Generation/Gen_Capacity_',temp_total_title,".csv"))
}
