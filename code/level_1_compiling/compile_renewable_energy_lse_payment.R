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
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSERPSPayment_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                    years[j],"_",cases[i],"/Results/ESR_Payment.csv");
    if (file.exists(temp_LSERPSPayment_fn)){
      temp_LSERPSPayment = read_csv(temp_LSERPSPayment_fn, col_types = cols()) %>%
        select(-AnnualSum) %>%
        pivot_longer(cols = -c(Zone),
                     names_to = 'Constraint') %>%
        mutate(item = 'esrpayment',
               case = cases[i], 
               year = years[j])
      if(!exists('LSERPSPayment')) {
        LSERPSPayment <- temp_LSERPSPayment;
      } else {
        LSERPSPayment <- rbind(LSERPSPayment, temp_LSERPSPayment);
      }
    }
  }
}
if(exists('LSERPSPayment')){
  write_csv(LSERPSPayment, paste0(RunFdr,"/CompiledResults/LSERPSPayment.csv"));
  print('finished compiling LSE RPS payment')
  print(Sys.time())
} else {
  print('there is no RPS_Cost.csv')
  print(Sys.time())
}
