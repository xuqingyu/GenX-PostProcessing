if(exists('modload_table')) {rm(modload_table,modload_table_withname)}
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_modifiedload_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                   years[j],"_",cases[i],"/Results/tfs_modifiedload.csv");
    if (file.exists(temp_modifiedload_fn)) {
      temp_modifiedload <- read_csv(temp_modifiedload_fn, col_types = cols(), skip = 1)[,-1] %>%
        pivot_longer(cols=everything(), names_to = 'Policy', values_to = 'MW') %>%
        mutate(case = cases[i],
               year = years[j])
      if (!exists('modload_table')){
        modload_table <- temp_modifiedload
        rm(temp_modifiedload,temp_modifiedload_fn)
      } else {
        modload_table <- rbind(modload_table, temp_modifiedload)
        rm(temp_modifiedload,temp_modifiedload_fn)
      }
    }
  }
}
modload_table_withname <- modload_table %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(modload_table_withname, paste0(RunFdr,"/CompiledResults/tfs_modified_load.csv"))
