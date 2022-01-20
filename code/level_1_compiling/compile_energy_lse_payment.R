#---------------------------------------#
# Combining LSE Energy Payment data -----
#---------------------------------------#
if (exists('LSEEnergyPayment')){
  rm('LSEEnergyPayment')
}
print('begin compiling LSE energy payment')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSEEnergyPayment_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                       years[j],"_",cases[i],"/Results/EnergyPayment.csv");
    if (file.exists(temp_LSEEnergyPayment_fn)) {
      temp_LSEEnergyPayment = read_csv(temp_LSEEnergyPayment_fn, 
                                       col_types = cols()) %>%
        mutate(case = cases[i],year = years[j])
      if(!exists('LSEEnergyPayment')) {
        LSEEnergyPayment <- temp_LSEEnergyPayment;
      } else {
        LSEEnergyPayment <- rbind(LSEEnergyPayment, temp_LSEEnergyPayment);
      }
    }
  }
}
if(exists('LSEEnergyPayment')){
  write_csv(LSEEnergyPayment, paste0(RunFdr,"/CompiledResults/LSEEnergyPayment.csv"));
  rm(temp_LSEEnergyPayment_fn,temp_LSEEnergyPayment,LSEEnergyPayment)
  print('finished compiling LSE energy payment')
  print(Sys.time())
} else {
  print('there are no EnergyPayment.csv')
  print(Sys.time())
}