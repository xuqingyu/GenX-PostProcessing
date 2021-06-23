#--------------------------------------#
#Combining LSE Capacity Payment data -----
#--------------------------------------#
if (exists('LSECapacityPayment')){
  rm('LSECapacityPayment')
}
print('begin compiling lse capacity payment')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSECapacityPayment_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/ReserveMarginCost.csv");
    if (file.exists(temp_LSECapacityPayment_fn)){
      temp_LSECapacityPayment = read.csv(temp_LSECapacityPayment_fn)
      end = dim(temp_LSECapacityPayment)[2]
      temp_LSECapacityPayment = pivot_longer(temp_LSECapacityPayment[-end], c(2:(end-1)),names_to = "item")
      temp_LSECapacityPayment$case = cases[i]
      temp_LSECapacityPayment$year = years[j]
      if(!exists('LSECapacityPayment')) {
        LSECapacityPayment <- temp_LSECapacityPayment;
      } else {
        LSECapacityPayment <- rbind(LSECapacityPayment, temp_LSECapacityPayment);
      }
    }
  }
}
if (exists('LSECapacityPayment')){
  write_csv(LSECapacityPayment, paste0(RunFdr,"/CompiledResults/LSECapacityPayment.csv"));
  print('finished compiling lse capacity payment')
} else {
  print('there are no ReserveMarginCost.csv')
}