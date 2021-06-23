#--------------------------------------#
# Combining RPS Payment data -----
#--------------------------------------#
if (exists('LSERPSPayment')){
  rm('LSERPSPayment')
}
if (exists('LSERPSElgibileLoad')){
  rm('LSERPSElgibileLoad')
}
print('begin compiling LSE RPS payment')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSERPSPayment_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/RPS_Cost.csv");
    if (file.exists(temp_LSERPSPayment_fn)){
      temp_LSERPSPayment = read.csv(temp_LSERPSPayment_fn)[,-c(2,3)] # Columns 2 and 3 are RPS elgibile load and storage loss
      end = dim(temp_LSERPSPayment)[2]
      temp_LSERPSPayment = pivot_longer(temp_LSERPSPayment[-end], c(2:(end-1)),names_to = "item")
      temp_LSERPSPayment$case = cases[i]
      temp_LSERPSPayment$year = years[j]
      
      temp_LSERPSElgibileLoad = read.csv(temp_LSERPSPayment_fn)[,c(1,2,3)] # Columns 2 and 3 are RPS elgibile load and storage loss
      temp_LSERPSElgibileLoad$case = cases[i]
      temp_LSERPSElgibileLoad$year = years[j]
      
      if(!exists('LSERPSPayment')) {
        LSERPSPayment <- temp_LSERPSPayment;
      } else {
        LSERPSPayment <- rbind(LSERPSPayment, temp_LSERPSPayment);
      }
      
      if(!exists('LSERPSElgibileLoad')) {
        LSERPSElgibileLoad <- temp_LSERPSElgibileLoad;
      } else {
        LSERPSElgibileLoad <- rbind(LSERPSElgibileLoad, temp_LSERPSElgibileLoad);
      }      
    }
  }
}
if(exists('LSERPSPayment')){
  write_csv(LSERPSPayment, paste0(RunFdr,"/CompiledResults/LSERPSPayment.csv"));
  write_csv(LSERPSElgibileLoad, paste0(RunFdr,"/CompiledResults/LSERPSElgibileLoad.csv"));
  print('finished compiling LSE RPS payment')
} else {
  print('there is no RPS_Cost.csv')
}
