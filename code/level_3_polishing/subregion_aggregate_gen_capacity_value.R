# Capacity Values ----
# source('./code/Header.R')
cap_values <- read_csv(paste0(RunFdr,"/CompiledResults/CapValue.csv"), col_types = cols()) %>%
  filter(`annual_sum` > 100) %>%
  na.omit() %>%
  left_join(resource_mapping) %>%
  rename(`zone` = Zone) %>%
  mutate(zone = as.factor(zone)) %>%
  left_join(zone_mapping) %>%
  left_join(cases_newnames,by = c('case' = 'case_description'))

for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  cap_values_subregion <- cap_values %>%
    filter(region %in% temp_total) %>%
    group_by(case, year, Reserve, Fuel, Scenario, TechSensitivity) %>%
    summarise(`Capacity Value` = mean(value)) %>%
    na.omit()
  write_csv(cap_values_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Capacity_Values_',temp_total_title,".csv"))
}
