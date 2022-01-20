#--------------------------------------#
#     Combining CO2 loadratecap  ------
#--------------------------------------#
if (exists('carbon_loadratecap')){
  rm('carbon_loadratecap')
}
print('begin compiling carbon emission cap load emission rate based')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_emission_loadratecap_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],
                                           "_",years[j],"_",cases[i],
                                           "/Inputs/CO2LoadRateCap.csv");
    if (file.exists(temp_emission_loadratecap_fn)){
      temp_carbon_loadratecap_raw = read_csv(paste0(temp_emission_loadratecap_fn), 
                                             col_types = cols())
      temp_NoCap = (dim(temp_carbon_loadratecap_raw)[2] - 2)/2
      temp_budgetcolumn = paste("CO_2_Max_LoadRate_",c(1:temp_NoCap),sep = "")
      temp_carbon_loadratecap_budget <- temp_carbon_loadratecap_raw %>%
        select(`Region description`,temp_budgetcolumn) %>%
        pivot_longer(temp_budgetcolumn, names_to = 'LoadRateCapID',values_to = 'LoadEmissionRate_Target_ton_per_MWh') %>%
        mutate(LoadRateCapID = str_remove(LoadRateCapID,'CO_2_Max_LoadRate_'))
      temp_membershipcolumn = paste("CO_2_Cap_Zone_",c(1:temp_NoCap),sep = "")
      temp_carbon_loadratecap_membership <- temp_carbon_loadratecap_raw %>%
        select(`Region description`,temp_membershipcolumn) %>%
        pivot_longer(temp_membershipcolumn, 
                     names_to = 'LoadRateCapID', 
                     values_to = 'Membership') %>%
        mutate(LoadRateCapID = str_remove(LoadRateCapID,'CO_2_Cap_Zone_'))
      temp_carbon_loadratecap <- left_join(temp_carbon_loadratecap_budget,
                                           temp_carbon_loadratecap_membership, 
                                           by = c('Region description','LoadRateCapID')) %>%
        mutate(case = cases[i], year = years[j])
      # temp_carbon_loadratecap$case = cases[i]
      # temp_carbon_loadratecap$year = years[j]
      if(!exists('carbon_loadratecap')){
        carbon_loadratecap <- temp_carbon_loadratecap;
      } else {
        carbon_loadratecap <- rbind(carbon_loadratecap, temp_carbon_loadratecap);
      }
    }
  }
}
if(exists('carbon_loadratecap')){
  write_csv(carbon_loadratecap, paste0(RunFdr,"/CompiledResults/CO2_loadratecap.csv"));
  rm(temp_emission_loadratecap_fn, temp_NoCap,temp_budgetcolumn,
     temp_carbon_loadratecap_raw,temp_carbon_loadratecap_budget,
     temp_membershipcolumn,temp_carbon_loadratecap_membership,temp_carbon_loadratecap)
  print('finish compiling carbon emission cap load emission rate based')
  print(Sys.time())
} else {
  print('there are no CO2LoadRateCap.csv')
  print(Sys.time())
}