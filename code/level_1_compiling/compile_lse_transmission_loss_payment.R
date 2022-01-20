#--------------------------------------#
#Combining LSE Transmission loss Payment data -----
#--------------------------------------#
if (exists('LSETransLossCost')){
  rm('LSETransLossCost')
}
print('begin compiling LSE transmission loss payment')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSETransLossCost_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],
                                       "_",years[j],"_",cases[i],
                                       "/Results/TransmissionLossCost.csv");
    if (file.exists(temp_LSETransLossCost_fn)){
      temp_LSETransLossCost = read_csv(temp_LSETransLossCost_fn, 
                                       col_types = cols())
      temp_LSETransLossCost$case = cases[i]
      temp_LSETransLossCost$year = years[j]
      if(!exists('LSETransLossCost')) {
        LSETransLossCost <- temp_LSETransLossCost;
      } else {
        LSETransLossCost <- rbind(LSETransLossCost, temp_LSETransLossCost);
      }
    }
  }
}
if (exists('LSETransLossCost')) {
  write_csv(LSETransLossCost, paste0(RunFdr,"/CompiledResults/LSETransLossCost.csv"));
  rm(temp_LSETransLossCost_fn, temp_LSETransLossCost,LSETransLossCost)
  print('finished compiling LSE transmission loss payment')
  print(Sys.time())
} else {
  print('there are no TransmissionLossCost.csv')
  print(Sys.time())
}
