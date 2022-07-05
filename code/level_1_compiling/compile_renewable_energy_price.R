#--------------------------------#
#Combining RPS Prices data -----
#--------------------------------#
if (exists('RPS_CES')){
  rm('RPS_CES')
}
print('begin compiling energy share requirement prices')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_RPS_CES_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_", 
                              years[j],"_",cases[i],"/Results/ESR_prices_penalty.csv");
    if (file.exists(temp_RPS_CES_fn)){
      temp_RPS_CES = read_csv(temp_RPS_CES_fn, col_types = cols()) %>%
        mutate(case = cases[i], year = years[j]) 
      # temp_RPS_CES$case = cases[i]
      # temp_RPS_CES$year = years[j]
      temp_RPS_CES$ID <- seq.int(nrow(temp_RPS_CES))
    }
    if(!exists('RPS_CES')) {
      RPS_CES <- temp_RPS_CES;
    } else {
      RPS_CES <- rbind(RPS_CES, temp_RPS_CES);
    }
  }
}
if(exists('RPS_CES')){
  write_csv(RPS_CES, paste0(RunFdr,"/CompiledResults/ESR_Prices.csv"));
  rm(temp_RPS_CES_fn, temp_RPS_CES,RPS_CES)
  print('finished energy share requirement prices')
  print(Sys.time())
} else {
  print('there are no ESR_prices.csv')
  print(Sys.time())
}
