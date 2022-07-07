#--------------------------------------#
# Combining RPS Payment storage loss-----
#--------------------------------------#
if (exists('ESRpaymentStorageloss')){
  rm('ESRpaymentStorageloss')
}
print('begin compiling LSE RPS storage loss payment')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSERPSPayment_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                    years[j],"_",cases[i],
                                    "/Results/ESR_StoragelossPayment.csv");
    if (file.exists(temp_LSERPSPayment_fn)){
      temp_LSERPSPayment = read_csv(temp_LSERPSPayment_fn, col_types = cols()) %>%
        select(-AnnualSum) %>%
        pivot_longer(cols = -c(Zone),
                     names_to = 'Constraint') %>%
        mutate(item = 'esrpaymentstorloss',
               case = cases[i], 
               year = years[j])
      if(!exists('ESRpaymentStorageloss')) {
        ESRpaymentStorageloss <- temp_LSERPSPayment;
      } else {
        ESRpaymentStorageloss <- rbind(ESRpaymentStorageloss, temp_LSERPSPayment);
      }
    }
  }
}
if(exists('ESRpaymentStorageloss')){
  write_csv(ESRpaymentStorageloss, 
            paste0(RunFdr,"/CompiledResults/ESR_PaymentStorageloss.csv"));
  print('finished compiling LSE RPS transmission loss payment')
  print(Sys.time())
} else {
  print('there is no ESR_StoragelossPayment.csv')
  print(Sys.time())
}
