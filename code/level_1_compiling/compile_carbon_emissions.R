#---------------------------------#
#     Combining CO2 results ----
#---------------------------------#
if (exists('carbon')){
  rm('carbon')
}
print('begin compiling CO2 emissions data')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_emission_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                               years[j],"_",cases[i],"/Results/emissions.csv");
    if (file.exists(temp_emission_fn)){
      temp_carbon = read_csv(paste0(temp_emission_fn), col_types = cols()) %>%
        select(-Total) %>%
        filter(Zone == 'AnnualSum') %>%
        rename(temp = Zone) %>%
        pivot_longer(!(temp),names_to = 'Zone') %>%
        select(-temp) %>%
        mutate(case = cases[i], 
               year = years[j])
    }
    if(!exists('carbon')) {
      carbon <- temp_carbon;
    } else {
      carbon <- rbind(carbon, temp_carbon);
    }
  }
}
if(exists('carbon')){
  write_csv(carbon, paste0(RunFdr,"/CompiledResults/CO2.csv"));
  print('finished compiling CO2 emissions data')
  print(Sys.time())
} else {
  print('there are no emissions.csv files')
  print(Sys.time())
}
