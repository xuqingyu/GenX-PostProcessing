# Calculate Renewable and Clean Energy for each subregion
# source('./code/Header.R')
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
}
