
# Output

if (exists('combined_power')){
  rm('combined_power');
}
print('begin compiling energy generation')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],
                                "/Inputs/Generators_data.csv");
    temp_power_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                            "_",cases[i],"/Results/power.csv");
    if (file.exists(temp_power_fn)){
      # The top 3 rows of the file are Resource, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_power_all = t(read_csv(temp_power_fn, 
                              col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_power_all) <- temp_power_all[1,] # make the row one as column name
      temp_power_all <- as_tibble(temp_power_all[-c(1, dim(temp_power_all)[1]),]) 
      # Remove the first row (as it as been set as column names)
      
      temp_generator <- read_csv(temp_generator_fn, col_types = cols());
      for (k in 1:n_tfs){
        policy_column = which(colnames(temp_generator) == paste0('RPSH_',k))
        cfe_rows = which(temp_generator[,policy_column] == 1)
        temp_power <- temp_power_all[cfe_rows, ] %>%
          mutate(Policy = k, case = cases[i], year = years[j])
        if(!exists('combined_power')) {
          combined_power <- temp_power;
          rm(temp_power, cfe_rows, policy_column)
        } else {
          combined_power <- rbind(combined_power, temp_power)
          rm(temp_power, cfe_rows, policy_column)
        }
      }
    }
    rm(temp_generator, temp_power_all,temp_generator_fn,temp_power_fn)
  }
}

if (exists('combined_power')){
  
  temp_total_title <- subreg
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == subreg]
  
  gen_power <- combined_power %>%
    mutate(AnnualSum = as.numeric(as.character(AnnualSum)),
           Zone = factor(Zone, levels = zone_mapping$zone)) %>%
    left_join(zone_mapping, by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Resource, Zone, Policy, AnnualSum) %>%
    left_join(resource_mapping) %>%
    filter(!(Fuel %in% storage_fuel)) %>%
    filter(!(Fuel %in% flexiload_list))

  genoutput_subregion <- gen_power%>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel,Policy) %>%
    summarize(AnnualOutput = sum(AnnualSum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Fuel, Scenario, `TechSensitivity`,Policy, AnnualOutput)
  write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',subreg,
                                       '/Generation/tfs_gen_output_',temp_total_title,".csv"))
  print('finished compiling energy generation')
  print(Sys.time())
  rm(gen_power, temp_total_title, temp_total,genoutput_subregion)
} else {
  print('there are no power.csv files')
  print(Sys.time())
}

