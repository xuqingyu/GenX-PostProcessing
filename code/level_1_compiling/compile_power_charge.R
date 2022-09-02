# Combining charge result----
if (exists('combined_charge')){
  rm('combined_charge');
}
print('begin compiling energy charge')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_charge_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Results/charge.csv");
    if (file.exists(temp_charge_fn)){
      # The top 3 rows of the file are Resource, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_charge = t(read_csv(temp_charge_fn, 
                              col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_charge) <- temp_charge[1,] # make the row one as column name
      temp_charge <- as_tibble(temp_charge[-c(1, dim(temp_charge)[1]),]) 
      # Remove the first row (as it as been set as column names)
      temp_charge$case = cases[i];
      temp_charge$year = years[j]
    }
    if(!exists('combined_charge')) {
      combined_charge <- temp_charge;
    } else {
      combined_charge <- rbind(combined_charge, temp_charge);
    }
  }
}
if(exists('combined_charge')){
  combined_charge$AnnualSum <- as.numeric(combined_charge$AnnualSum);
  combined_charge <- left_join(combined_charge, zone_mapping, 
                               by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Resource, Zone, # Fuel, 
           AnnualSum);
  charge_for_settlement <- combined_charge;
  write_csv(charge_for_settlement, 
            paste0(RunFdr,"/CompiledResults/charge_for_settlement.csv"));
  # if (identical(years, c(2030, 2040, 2050))){
  #   combined_charge_temp1 <- subset(combined_charge,Fuel == "ZCF") %>%
  #     mutate(Resource = paste(Resource,"_ZCF",sep = ""));
  #   combined_charge <- rbind(combined_charge_temp1, 
  #                            subset(combined_charge, Fuel != "ZCF"));
  #   rm(combined_charge_temp1)
  # }
  # combined_charge <- subset(combined_charge,select = -c(Fuel));
  write_csv(combined_charge, paste0(RunFdr,"/CompiledResults/charge.csv"));
  rm(temp_charge_fn, temp_charge, combined_charge, charge_for_settlement)
  # rm(temp_generator_fn, temp_generator)
  print('finished compiling energy charge')
  print(Sys.time())
} else {
  print('there are no charge.csv files')
  print(Sys.time())
}
