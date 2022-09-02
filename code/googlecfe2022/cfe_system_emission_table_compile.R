

if(exists('systemload_emission_table')) {rm(systemload_emission_table,systemload_emission_table_withname)}
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_systememission_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                   years[j],"_",cases[i],"/Results/tfs_system_withdraw_emissions.csv");
    temp_systememission <- read_csv(temp_systememission_fn, col_types = cols()) %>%
      select(emissions_part_load_plan_a, 
             emissions_nonpart_load_plan_a) %>%
      rename(participating_load_emission = emissions_part_load_plan_a,
             emission_rest_of_local_w_import = emissions_nonpart_load_plan_a) %>%
      mutate(emission_local_n_import = participating_load_emission + emission_rest_of_local_w_import) %>%
      mutate(case = cases[i],
             year = years[j])
    if (!exists('systemload_emission_table')){
      systemload_emission_table <- temp_systememission
      rm(temp_systememission,temp_systememission_fn)
    } else {
      systemload_emission_table <- rbind(systemload_emission_table, temp_systememission)
      rm(temp_systememission,temp_systememission_fn)
    }
  }
}
systemload_emission_table_withname <- systemload_emission_table %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(systemload_emission_table_withname, paste0(RunFdr,"/CompiledResults/tfs_system_emissions.csv"))
