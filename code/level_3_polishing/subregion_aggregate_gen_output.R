# Generation Output
# # 
# source('./code/Header.R')
gen_power <- read_csv(paste0(RunFdr,'CompiledResults/power.csv'), 
                      col_types = cols()) %>%
  left_join(resource_mapping) %>%
  filter(!(Fuel %in% storage_fuel)) %>%
  filter(!(Fuel %in% flexiload_list)) %>%
  na.omit()
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  genoutput_subregion <- gen_power %>%
    filter(Region %in% temp_total) %>%
    group_by(case, year, Fuel) %>%
    summarize(AnnualOutput = sum(AnnualSum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualOutput)
  write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                       '/Generation/Gen_Output_',temp_total_title,".csv"))
  rm(genoutput_subregion)
}
rm(gen_power)
