# Compile Export

if (exists('import_export')){
  rm('import_export');
}
print('begin compiling cfe export')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_export_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                             "_",cases[i],"/Results/tfs_tfsexport.csv");
    temp_timeweight_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                             "_",cases[i],"/Results/time_weights.csv");
    if (file.exists(temp_export_fn)){
      # The top 3 rows of the file are Resource, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_flow = read_csv(temp_export_fn, 
                               col_names = T, col_types = cols())[-1, -1];
      temp_export_pos <- temp_flow
      temp_export_pos[temp_export_pos <= 0] <- 0
      temp_export_neg <- temp_flow
      temp_export_neg[temp_export_neg >= 0] <- 0
      temp_timeweight = read_csv(temp_timeweight_fn,col_types = cols())[,-1]
      temp_totalexport = t(-1*as.matrix(temp_export_pos)) %*% as.matrix(temp_timeweight) %>%
        as_tibble_col(column_name = 'AnnualSum') %>%
        mutate(Policy = c(1:ncol(temp_flow)),
               Fuel = 'TotalExport', case = cases[i], year = years[j])
      temp_totalimport = t(-1*as.matrix(temp_export_neg)) %*% as.matrix(temp_timeweight) %>%
        as_tibble_col(column_name = 'AnnualSum') %>%
        mutate(Policy = c(1:ncol(temp_flow)),
               Fuel = 'TotalImport', case = cases[i], year = years[j])
      
      temp_import_export = rbind(temp_totalexport,temp_totalimport)
      rm(temp_flow, temp_export_pos, temp_export_neg,temp_timeweight)
      rm(temp_totalexport, temp_totalimport)
      # Remove the first row (as it as been set as column names)
      if(!exists('import_export')) {
        import_export <- temp_import_export;
        rm(temp_import_export)
      } else {
        import_export <- rbind(import_export, temp_import_export)
        rm(temp_import_export)
      }
    }
    rm(temp_export_fn,temp_timeweight_fn)
  }
}


import_export_final <- import_export %>%
  left_join(cases_newnames, by = c('case' = 'case_description')) %>%
  select(case,year,Fuel, Scenario, `TechSensitivity`, Policy, AnnualSum) %>%
  write_csv(paste0(RunFdr,"/CompiledResults/tfs_total_import_export.csv"))

rm(import_export_final, import_export)
