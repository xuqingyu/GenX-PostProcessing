
# Combining Curtailment result----
if (exists('combined_REcurtail')){
  rm('combined_REcurtail');
}
print('begin compiling curtailment')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],
                                "/Inputs/Generators_data.csv");
    temp_curtail_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                              "_",cases[i],"/Results/curtail.csv");
    if (file.exists(temp_curtail_fn)){
      
      # The top 3 rows of the file are Resource, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_REcurtail = t(read_csv(temp_curtail_fn, 
                               col_names = F, n_max = 3, col_types = cols()))
      # make the row one as column name
      colnames(temp_REcurtail) <- temp_REcurtail[1,] 
      # Remove the first row (as it as been set as column names)      
      temp_REcurtail <- as_tibble(temp_REcurtail[-c(1, dim(temp_REcurtail)[1]),]) 
      temp_REcurtail$case = cases[i];
      temp_REcurtail$year = years[j]
    }
    if(!exists('combined_REcurtail')) {
      combined_REcurtail <- temp_REcurtail;
    } else {
      combined_REcurtail <- rbind(combined_REcurtail, temp_REcurtail);
    }
  }
}
if (exists('combined_REcurtail')){
  combined_REcurtail$AnnualSum <- as.numeric(combined_REcurtail$AnnualSum);
  combined_REcurtail <- left_join(combined_REcurtail, zone_mapping, 
                                  by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Resource, Zone, # Fuel, 
           AnnualSum);
  write_csv(combined_REcurtail, 
            paste0(RunFdr,"/CompiledResults/renewablecurtail.csv"));
  rm(temp_curtail_fn, temp_REcurtail, combined_REcurtail)
  # rm(temp_generator_fn, temp_generator)
  print('finished compiling curtailment')
  print(Sys.time())
} else {
  print('there are no curtail.csv files')
  print(Sys.time())
}

