

#---------------------------------#
#     Combining CO2 results ----
#---------------------------------#
if (exists('carbon')){
  rm('carbon')
}
print('begin compiling CO2 emissions data')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_emission_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/emissions.csv");
    if (file.exists(temp_emission_fn)){
      temp_carbon = read_csv(paste0(temp_emission_fn)) %>%
        select(-Total) %>%
        filter(Zone == 'Sum') %>%
        rename(temp = Zone) %>%
        pivot_longer(!(temp),names_to = 'Zone') %>%
        select(-temp)
      temp_carbon$case = cases[i]
      temp_carbon$year = years[j]
    }
    if(!exists('carbon'))
    {
      carbon <- temp_carbon;
    }
    else
    {
      carbon <- rbind(carbon, temp_carbon);
    }
  }
}
if(exists('carbon')){
  write_csv(carbon, paste0(RunFdr,"/CompiledResults/CO2.csv"));
  print('finished compiling CO2 emissions data')
} else {
  print('there are no emissions.csv files')
}