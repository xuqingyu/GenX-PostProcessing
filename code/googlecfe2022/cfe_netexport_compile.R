# Export

if (exists('combined_export')){
  rm('combined_export');
}
print('begin compiling cfe export')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_export_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                            "_",cases[i],"/Results/tfs_tfsexport.csv");
    if (file.exists(temp_export_fn)){
      # The top 3 rows of the file are Resource, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_export = t(read_csv(temp_export_fn, 
                                  col_names = F, n_max = 2, col_types = cols()));
      colnames(temp_export) <- temp_export[1,] # make the row one as column name
      temp_export <- as_tibble(temp_export[-1,])%>%
        mutate(case = cases[i], year = years[j])
      colnames(temp_export)[1] = 'Policy'
      temp_export <- temp_export %>%
        mutate(Policy = as.numeric(Policy))
      # Remove the first row (as it as been set as column names)
      if(!exists('combined_export')) {
        combined_export <- temp_export;
        rm(temp_export)
      } else {
        combined_export <- rbind(combined_export, temp_export)
        rm(temp_export)
      }
    }
    rm(temp_export_fn)
  }
}

combined_export_pos = filter(combined_export, AnnualSum >0) %>%
  mutate(AnnualSum = -1* as.numeric(AnnualSum)) %>%
  mutate(Fuel = 'NetExport')
combined_export_neg = filter(combined_export, AnnualSum <0) %>%
  mutate(AnnualSum = -1* as.numeric(AnnualSum)) %>%
  mutate(Fuel = 'NetImport')
combined_export_final <- rbind(combined_export_pos, combined_export_neg) %>%
  left_join(cases_newnames, by = c('case' = 'case_description')) %>%
  select(case,year,Fuel, Scenario, `TechSensitivity`,Policy,AnnualSum) %>%
  write_csv(paste0(RunFdr,"/CompiledResults/tfs_netexport.csv"))

rm(combined_export_pos, combined_export_neg, combined_export_final)
