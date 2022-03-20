
#---------------------------------------------------#
#     Combining CO2 Load Rate Cap Price results ----
#---------------------------------------------------#
if (exists('carbon_loadrate_price')){
  rm('carbon_loadrate_price')
}
print('begin compiling load-emission-rate-based carbon cap prices')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_co2_loadrate_price_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                         years[j],"_",cases[i],
                                         "/Results/CO2Price_loadrate.csv");
    if (file.exists(temp_co2_loadrate_price_fn)){
      temp_carbon_loadrate_price = read_csv(paste0(temp_co2_loadrate_price_fn),
                                            col_types = cols()) %>%
        pivot_longer(cols=!c(Zone), names_to = 'Constraint',
                     values_to = 'Price') %>%
        mutate(case = cases[i], year = years[j])
      if(!exists('carbon_loadrate_price')){
        carbon_loadrate_price <- temp_carbon_loadrate_price;
      }
      else{
        carbon_loadrate_price <- rbind(carbon_loadrate_price, temp_carbon_loadrate_price);
      }
    }
  }
}
if(exists('carbon_loadrate_price')){
  write_csv(carbon_loadrate_price, paste0(RunFdr,"/CompiledResults/CO2_LoadRate_Price.csv"));
  print('finished compiling load-emission-rate-based carbon cap prices')
  print(Sys.time())
} else {
  print('there are no CO2Price_loadrate.csv files')
  print(Sys.time())
}