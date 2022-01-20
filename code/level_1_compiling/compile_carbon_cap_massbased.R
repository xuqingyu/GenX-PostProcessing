#---------------------------------#
#     Combining CO2 masscap  ------
#---------------------------------#
if (exists('carbon_masscap')){
  rm('carbon_masscap')
}
print('begin compiling CO2 emission mass cap data')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_emission_masscap_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],
                                       "_",years[j],"_",cases[i],"/Inputs/CO2Cap.csv");
    if (file.exists(temp_emission_masscap_fn)){
      temp_carbon_masscap_raw = read_csv(paste0(temp_emission_masscap_fn),
                                         col_types = cols())
      temp_NoCap = (dim(temp_carbon_masscap_raw)[2] - 2)/2
      temp_budgetcolumn = paste("CO_2_Max_Mtons_", c(1:temp_NoCap), sep = "")
      temp_carbon_masscap_budget <- temp_carbon_masscap_raw %>%
        select(`Region description`,temp_budgetcolumn) %>%
        pivot_longer(temp_budgetcolumn, names_to = 'MassCapID', 
                     values_to = 'Mass_Budget_Mtons') %>%
        mutate(MassCapID = str_remove(MassCapID,'CO_2_Max_Mtons_'))
      temp_membershipcolumn = paste("CO_2_Cap_Zone_",c(1:temp_NoCap),sep = "")
      temp_carbon_masscap_membership <- temp_carbon_masscap_raw %>%
        select(`Region description`,temp_membershipcolumn) %>%
        pivot_longer(temp_membershipcolumn, 
                     names_to = 'MassCapID',values_to = 'Membership') %>%
        mutate(MassCapID = str_remove(MassCapID,'CO_2_Cap_Zone_'))
      temp_carbon_masscap <- left_join(temp_carbon_masscap_budget,
                                       temp_carbon_masscap_membership, 
                                       by = c('Region description','MassCapID'))
      temp_carbon_masscap$case = cases[i]
      temp_carbon_masscap$year = years[j]
      if(!exists('carbon_masscap')) {
        carbon_masscap <- temp_carbon_masscap;
      } else {
        carbon_masscap <- rbind(carbon_masscap, temp_carbon_masscap);
      }
    }
  }
}
if(exists('carbon_masscap')){
  write_csv(carbon_masscap, paste0(RunFdr,"/CompiledResults/CO2_masscap.csv"));
  rm(carbon_masscap,temp_carbon_masscap, temp_emission_masscap_fn,
     temp_carbon_masscap_raw, temp_NoCap,temp_budgetcolumn,temp_carbon_masscap_budget,
     temp_membershipcolumn,temp_carbon_masscap_membership)
  print('finish compiling CO2 emission mass cap data')
  print(Sys.time())
} else {
  print('there are no CO2Cap.csv')
  print(Sys.time())
}
