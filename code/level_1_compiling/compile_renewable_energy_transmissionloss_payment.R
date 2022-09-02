#--------------------------------------#
# Combining RPS Payment transmission loss-----
#--------------------------------------#
if (exists('ESRpaymentTransmissionloss')){
  rm('ESRpaymentTransmissionloss')
}
print('begin compiling LSE RPS transmission loss payment')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSERPSPayment_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                    years[j],"_",cases[i],
                                    "/Results/ESR_TransmissionlossPayment.csv");
    if (file.exists(temp_LSERPSPayment_fn)){
      temp_LSERPSPayment = read_csv(temp_LSERPSPayment_fn, col_types = cols()) %>%
        select(-AnnualSum) %>%
        pivot_longer(cols = -c(Zone),
                     names_to = 'Constraint') %>%
        mutate(item = 'esrpaymenttransloss',
               case = cases[i], 
               year = years[j])
      if(!exists('ESRpaymentTransmissionloss')) {
        ESRpaymentTransmissionloss <- temp_LSERPSPayment;
      } else {
        ESRpaymentTransmissionloss <- rbind(ESRpaymentTransmissionloss, temp_LSERPSPayment);
      }
    }
  }
}
if(exists('ESRpaymentTransmissionloss')){
  write_csv(ESRpaymentTransmissionloss, 
            paste0(RunFdr,"/CompiledResults/ESR_PaymentTransmissionloss.csv"));
  print('finished compiling LSE RPS transmission loss payment')
  print(Sys.time())
} else {
  print('there is no ESR_TransmissionlossPayment.csv')
  print(Sys.time())
}
