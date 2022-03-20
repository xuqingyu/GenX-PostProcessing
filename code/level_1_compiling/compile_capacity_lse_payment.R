#--------------------------------------#
#Combining LSE Capacity Payment data -----
#--------------------------------------#
if (exists('LSECapacityPayment')){
  rm('LSECapacityPayment')
}
print('begin compiling lse capacity payment')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSECapacityPayment_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                         years[j],"_",cases[i],
                                         "/Results/ReserveMarginPayment.csv");
    temp_LSECapacityDemandReponseSavings_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                         years[j],"_",cases[i],
                                         "/Results/ReserveMarginDemandResponseSaving.csv");
    if (file.exists(temp_LSECapacityPayment_fn)){
      temp_LSECapacityPayment = read_csv(temp_LSECapacityPayment_fn, 
                                         col_types = cols()) %>%
        select(-AnnualSum) %>%
        pivot_longer(cols = -c(Zone),
                     names_to = 'Reserve') %>%
        mutate(item = 'payment',
               case = cases[i], 
               year = years[j])
      temp_LSECapacityDemandReponseSavings = read_csv(temp_LSECapacityDemandReponseSavings_fn, 
                                         col_types = cols()) %>%
        select(-AnnualSum) %>%
        pivot_longer(cols = -c(Zone),
                     names_to = 'Reserve') %>%
        mutate(item = 'demandreponsesaving',
               value = -1 * value,
               case = cases[i], 
               year = years[j])
      
      # temp_LSECapacityPayment = pivot_longer(temp_LSECapacityPayment[-end], 
      #                                        c(2:(end-1)),
      #                                        names_to = "item") %>%
      #   mutate(case = cases[i], year = years[j])
      if(!exists('LSECapacityPayment')) {
        LSECapacityPayment <- rbind(temp_LSECapacityPayment,temp_LSECapacityDemandReponseSavings);
      } else {
        LSECapacityPayment <- rbind(LSECapacityPayment, temp_LSECapacityPayment,temp_LSECapacityDemandReponseSavings);
      }
    }
  }
}
if (exists('LSECapacityPayment')){
  write_csv(LSECapacityPayment, paste0(RunFdr,"/CompiledResults/LSECapacityPayment.csv"));
  print('finished compiling lse capacity payment')
  print(Sys.time())
} else {
  print('there are no ReserveMarginCost.csv')
  print(Sys.time())
}