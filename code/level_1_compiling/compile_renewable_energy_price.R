#--------------------------------#
#Combining RPS Prices data -----
#--------------------------------#
if (exists('RPS_CES')){
  rm('RPS_CES')
}
print('begin compiling renewable energy prices')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_RPS_CES_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/RPS_prices.csv");
    if (file.exists(temp_RPS_CES_fn)){
      temp_RPS_CES = read.csv(temp_RPS_CES_fn)
      temp_RPS_CES$case = cases[i]
      temp_RPS_CES$year = years[j]
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
  write_csv(RPS_CES, paste0(RunFdr,"/CompiledResults/RPS_CES.csv"));
  rm(temp_RPS_CES_fn, temp_RPS_CES,RPS_CES)
  print('finished compiling renewable energy prices')
} else {
  print('there are no RPS_prices.csv')
}
