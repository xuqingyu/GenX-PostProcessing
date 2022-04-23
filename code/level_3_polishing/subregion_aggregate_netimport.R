# Calculate net import


for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  
  demand <- read_csv(paste0(RunFdr, '/CompiledResults/Total_load_summary.csv'), col_types = cols()) %>%
    mutate(zone = str_remove_all(Region, 'Load_MW_z'),
           zone = factor(zone, levels = zone_mapping$zone),
           AnnualLoad = AnnualLoad * 1e6) %>%
    select(-Region) %>%
    left_join(zone_mapping) %>%
    rename(Zone = zone, Region = region) %>%
    filter(Region %in% temp_total) %>%
    group_by(case, year) %>%
    summarize(AnnualLoad = sum(AnnualLoad))
  nse <- read_csv(paste0(RunFdr, '/CompiledResults/nse.csv'), col_types = cols()) %>%
    filter(Region %in% temp_total) %>%
    group_by(case, year) %>%
    summarize(AnnualNSE = sum(AnnualSum))
  zonaltloss <- read_csv(paste0(RunFdr, '/CompiledResults/zonaltransmissionloss.csv'), col_types = cols())  %>%
    filter(Region %in% temp_total) %>%
    group_by(case, year) %>%
    summarize(AnnualTloss = sum(AnnualSum))
  gen_output <- read_csv(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Profit_',temp_total_title,".csv"), col_types = cols()) %>%
    select(case,year, Scenario, TechSensitivity, AnnualOutput, AnnualCharge) %>%
    group_by(case, year, Scenario, TechSensitivity) %>%
    summarize(AnnualOutput = sum(AnnualOutput),
              AnnualCharge = sum(AnnualCharge))
  netimport <- left_join(demand, nse) %>%
    left_join(zonaltloss) %>%
    left_join(gen_output) %>%
    mutate(AnnualImport = AnnualLoad - AnnualNSE + AnnualTloss + AnnualCharge - AnnualOutput) %>%
    select(case, year, Scenario, TechSensitivity, AnnualImport) %>%
    write_csv(paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/NetImport_',temp_total_title,".csv"))
  
}
