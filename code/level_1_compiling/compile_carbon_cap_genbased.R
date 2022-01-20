#--------------------------------------#
#     Combining CO2 genratecap  -------
#--------------------------------------#
if (exists('carbon_genratecap')){
  rm('carbon_genratecap')
}
print('begin compiling carbon emission cap generation emission rate based')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_emission_genratecap_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                          years[j],"_",cases[i],"/Inputs/CO2GenRateCap.csv");
    if (file.exists(temp_emission_genratecap_fn)){
      temp_carbon_genratecap_raw = read_csv(paste0(temp_emission_genratecap_fn), 
                                            col_types = cols())
      temp_NoCap = (dim(temp_carbon_genratecap_raw)[2] - 2)/2
      temp_budgetcolumn = paste("CO_2_Max_GenRate_",c(1:temp_NoCap),sep = "")
      temp_carbon_genratecap_budget <- temp_carbon_genratecap_raw %>%
        select(`Region description`,temp_budgetcolumn) %>%
        pivot_longer(temp_budgetcolumn, names_to = 'GenRateCapID', 
                     values_to = 'GenEmissionRate_Target_ton_per_MWh') %>%
        mutate(GenRateCapID = str_remove(GenRateCapID,'CO_2_Max_GenRate_'))
      temp_membershipcolumn = paste("CO_2_Cap_Zone_",c(1:temp_NoCap),sep = "")
      temp_carbon_genratecap_membership <- temp_carbon_genratecap_budget %>%
        select(`Region description`,temp_membershipcolumn) %>%
        pivot_longer(temp_membershipcolumn, names_to = 'GenRateCapID', 
                     values_to = 'Membership') %>%
        mutate(GenRateCapID = str_remove(GenRateCapID,'CO_2_Cap_Zone_'))
      temp_carbon_genratecap <- left_join(temp_carbon_genratecap_budget, 
                                          temp_carbon_genratecap_membership, 
                                          by = c('Region description','GenRateCapID')) %>%
        mutate(case = cases[i], year = years[j])
      # temp_carbon_genratecap$case = cases[i]
      # temp_carbon_genratecap$year = years[j]
      if(!exists('carbon_genratecap')){
        carbon_genratecap <- temp_carbon_genratecap;
      } else {
        carbon_genratecap <- rbind(carbon_genratecap, temp_carbon_genratecap);
      }
    }
  }
}
if(exists('carbon_genratecap')){
  write_csv(carbon_genratecap, paste0(RunFdr,"/CompiledResults/CO2_genratecap.csv"));
  rm(temp_emission_genratecap_fn, temp_carbon_genratecap_raw,temp_NoCap,temp_budgetcolumn,
     temp_carbon_genratecap_budget,temp_membershipcolumn,temp_carbon_genratecap_membership,
     temp_carbon_genratecap,carbon_genratecap)
  print('finish compiling carbon emission cap generation emission rate based')
  print(Sys.time())
} else {
  print('there are no CO2GenRateCap.csv')
  print(Sys.time())
}