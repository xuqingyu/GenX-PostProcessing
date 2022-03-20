
if(exists('cfe_gents_table')) {rm(cfe_gents_table, cfe_gents_table_withname)}
for ( i in 1:length(cases)) {
  for (j in 1:length(years)) {
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],
                                "/Inputs/Generators_data.csv");
    temp_power_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                            "_",cases[i],"/Results/power.csv");
    if (file.exists(temp_power_fn)){
      # skip the first three columns
      power_ts_all = read_csv(temp_power_fn, col_names = F, skip = 3, col_types = cols())
      # remove the last column of total and the first column of time
      power_ts_all = power_ts_all[,-c(1,ncol(power_ts_all))]
      power_ts_transposed <- t(power_ts_all)
      colnames(power_ts_transposed) = c(1:ncol(power_ts_transposed))
      power_ts_transposed <- as_tibble(power_ts_transposed)
      
      temp_generator <- read_csv(temp_generator_fn, col_types = cols()) %>%
        select(-Fuel) %>%
        left_join(resource_mapping)
      power_ts_transposed <- power_ts_transposed %>%
        mutate(Resource = temp_generator$Resource,
               Fuel = temp_generator$Fuel)
      for (k in 1:n_tfs){
        policy_column = which(colnames(temp_generator) == paste0('RPSH_',k))
        cfe_rows = which(temp_generator[,policy_column] == 1) 
        temp_power_ts <- power_ts_transposed[cfe_rows, ] %>%
          filter(!(Fuel %in% storage_fuel)) %>%
          filter(!(Fuel %in% flexiload_list)) %>%
          na.omit() %>%
          pivot_longer(cols = -c(Resource, Fuel),names_to = "Time_Index", values_to = "MW") %>%
          group_by(Fuel, Time_Index) %>%
          summarize(MW = sum(MW)) %>%
          mutate(Policy = k, case = cases[i], year = years[j])
        if(!exists('cfe_gents_table')) {
          cfe_gents_table <- temp_power_ts;
          rm(temp_power_ts, cfe_rows, policy_column)
        } else {
          cfe_gents_table <- rbind(cfe_gents_table, temp_power_ts)
          rm(temp_power_ts, cfe_rows, policy_column)
        }
      }
    }
    rm(temp_generator, power_ts_all, power_ts_transposed,temp_generator_fn, temp_power_fn)
  }
}
cfe_gents_table_withname <- cfe_gents_table %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))

write_csv(cfe_gents_table_withname, paste0(RunFdr,"/CompiledResults/tfs_gents_table.csv"))
rm(cfe_gents_table_withname, cfe_gents_table)

