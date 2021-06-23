#--------------------------------------#
# Combining CO2 Tax data -----
#--------------------------------------#
if (exists('LSECO2Tax')){
  rm('LSECO2Tax', 'GenCO2Tax')
}
print('begin compiling CO2 Tax data')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSECO2Tax_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/CO2Cost_tax.csv");
    if (file.exists(temp_LSECO2Tax_fn)){
      temp_LSECO2Tax = read_csv(temp_LSECO2Tax_fn) %>%
        group_by(zone) %>%
        summarize(value = sum(Sum)) %>%
        mutate(item = 'CO2Tax',
               case = cases[i],
               year = years[j])
      temp_GenCO2Tax = read_csv(temp_LSECO2Tax_fn)
        rename(value = Sum) %>%
        mutate(item = 'CO2Tax',
               case = cases[i],
               year = years[j])
      if (!exists('LSECO2Tax')) {
        LSECO2Tax <- temp_LSECO2Tax;
        GenCO2Tax <- temp_GenCO2Tax;
      } else {
        LSECO2Tax <- rbind(LSECO2Tax, temp_LSECO2Tax);
        GenCO2Tax <- rbind(GenCO2Tax, temp_GenCO2Tax)
      }
    }
  }
}
if (exists('LSECO2Tax')) {
  write_csv(LSECO2Tax, paste0(RunFdr,"/CompiledResults/LSECO2Revenue_tax.csv"));
  write_csv(GenCO2Tax, paste0(RunFdr,"/CompiledResults/GenCO2Cost_tax.csv"));
  rm(temp_LSECO2Tax_fn, temp_LSECO2Tax,temp_GenCO2Tax,LSECO2Tax,GenCO2Tax)
  print('finised compiling CO2 Tax data')
} else {
  print('there are no CO2Cost_tax.csv')
}