#----------------------------------------------#
#     Combining CO2 Mass Cap Price results ----
#----------------------------------------------#
if (exists('carbon_mass_price')){
  rm('carbon_mass_price')
}
print('begin compiling mass-based carbon cap prices')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_co2_mass_price_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                     years[j],"_",cases[i],"/Results/CO2Price_mass.csv");
    if (file.exists(temp_co2_mass_price_fn)){
      temp_carbon_mass_price = read_csv(paste0(temp_co2_mass_price_fn), 
                                        col_types = cols(),col_names = F);
      n_constraint <- dim(temp_carbon_mass_price)[2]-1
      colnames(temp_carbon_mass_price) <- c('Zone',paste('CO2_Price_',c(1:n_constraint),sep=''))
      temp_carbon_mass_price <- temp_carbon_mass_price %>%
        pivot_longer(cols=!c(Zone),
                     names_to = 'Constraint',
                     values_to = 'Price') %>%
        mutate(case = cases[i], year = years[j])
      # temp_carbon_mass_price$case = cases[i]
      # temp_carbon_mass_price$year = years[j]
      if(!exists('carbon_mass_price')){
        carbon_mass_price <- temp_carbon_mass_price;
      }
      else{
        carbon_mass_price <- rbind(carbon_mass_price, temp_carbon_mass_price);
      }
    }
  }
}
if (exists('carbon_mass_price')){
  write_csv(carbon_mass_price, paste0(RunFdr,"/CompiledResults/CO2_Mass_Price.csv"));
  print('finished compiling mass-based carbon cap prices')
  print(Sys.time())
} else {
  print('there are no CO2Price_mass.csv files')
  print(Sys.time())
}
