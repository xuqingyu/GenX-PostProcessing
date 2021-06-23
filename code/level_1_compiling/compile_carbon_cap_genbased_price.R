#---------------------------------------------------#
#     Combining CO2 Gen Rate Cap Price results ----
#---------------------------------------------------#
if (exists('carbon_genrate_price')){
  rm('carbon_genrate_price')
}
print('begin compiling generation-emission-rate-based carbon cap prices')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_co2_genrate_price_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/CO2Price_genrate.csv");
    if (file.exists(temp_co2_genrate_price_fn)){
      temp_carbon_genrate_price = read_csv(paste0(temp_co2_genrate_price_fn)) %>%
        pivot_longer(cols=!c(Zone),names_to = 'Constraint',values_to = 'Price')
      temp_carbon_genrate_price$case = cases[i]
      temp_carbon_genrate_price$year = years[j]
      if(!exists('carbon_genrate_price')){
        carbon_genrate_price <- temp_carbon_genrate_price;
      }
      else{
        carbon_genrate_price <- rbind(carbon_genrate_price, temp_carbon_genrate_price);
      }
    }
  }
}
if(exists('carbon_genrate_price')){
  write_csv(carbon_genrate_price, paste0(RunFdr,"/CompiledResults/CO2_GenRate_Price.csv"));
  print('finished compiling generation-emission-rate-based carbon cap prices')
} else {
  print('there are no CO2Price_genrate.csv')
}
