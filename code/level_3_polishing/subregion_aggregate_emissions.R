# Subregion emissions ----
for ( i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  emission_subregion <- read_csv(paste0(RunFdr,'/CompiledResults/CO2.csv'), col_types = cols());
  emission_subregion$Zone <- as.factor(emission_subregion$Zone);
  emission_subregion <- emission_subregion %>%
    left_join(zone_mapping,by=c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    filter(Region %in% temp_total) %>%
    group_by(case,year) %>%
    summarize(`Emissions (Mtons)` = sum(value)/1e6) %>%
    left_join(cases_newnames, by = c('case' = 'case_description'))
  # Calculate the total generation of each case-year
  gen_profit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',
                                    Subregions[i],'/Generation/Gen_Profit_',
                                    temp_total_title,".csv")
  total_gen <- read_csv(gen_profit_subregion_fn, col_types = cols()) %>%
    filter(!(Fuel %in% storage_fuel)) %>%
    group_by(case,year,Scenario, TechSensitivity) %>%
    summarize(TotalGen = sum(AnnualOutput))
  # Calculate the total load of each case-year
  # lse_payment_subregion_fn <- paste0(RunFdr,'/CompiledResults/',
  #                                    Subregions[i],'/Load/LSE_Payment_',
  #                                    temp_total_title,"_with2019_and_DG.csv")
  # total_load_subregion <- read_csv(lse_payment_subregion_fn) %>%
  #   select(case,year,Scenario, TechSensitivity, AnnualLoad,`Gross Total`)  %>%
  #   filter(year != 2019)
  # Calculate emission rate
  emission_subregion <- left_join(emission_subregion, total_gen) %>% 
    # left_join(total_load_subregion) %>%
    mutate(`Generation Emissions Rate (Ton/MWh)` = `Emissions (Mtons)`/TotalGen*1e6,
           # `Load Emissions Rate (Ton/MWh)` = `Emissions (Mtons)`/`Gross Total`*1e6
           ) %>%
    select(case, year, Scenario, TechSensitivity, 
           `Emissions (Mtons)`,
           `Generation Emissions Rate (Ton/MWh)`,
           # `Load Emissions Rate (Ton/MWh)`
           )
  write_csv(emission_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Emissions/Emissions_',temp_total_title,".csv"))
}
# # Add in 2019 ----
# for (i in 1:n_subregions) {
#   temp_total_title <- Subregions[i]
#   temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
#   emissions_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Emissions/Emissions_',temp_total_title,".csv")
#   if (file.exists(emissions_subregion_fn)){
#     emissions_subregion <- read_csv(emissions_subregion_fn)
#     case_temp <- unique(select(emissions_subregion,case))
#     n_case_temp <- dim(case_temp)[1]
#     if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
#       if (temp_total_title == 'New Jersey'){
#         emissions_2019_base <- read_csv('./data/Emissions_2019_NJ.csv')
#       } else if (temp_total_title == 'PJM'){
#         emissions_2019_base <- read_csv('./data/Emissions_2019_PJM.csv')
#       }
#       for (j in 1:n_case_temp) {
#         temp_emissions_2019_base <- emissions_2019_base;
#         temp_emissions_2019_base$case <- case_temp$case[j]
#         temp_emissions_2019_base <- left_join(temp_emissions_2019_base, cases_newnames, by = c('case' = 'case_description')) %>%
#           select(case, year, Scenario, TechSensitivity, 
#                  `Emissions (Mtons)`,`Generation Emissions Rate (Ton/MWh)`,`Load Emissions Rate (Ton/MWh)`)
#         emissions_subregion <- rbind(emissions_subregion,temp_emissions_2019_base)
#       }
#     }
#     write_csv(emissions_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Emissions/Emissions_',temp_total_title,"_with2019.csv"))
#   }
# }

