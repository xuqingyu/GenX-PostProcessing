
if(exists('part_emission_table')) {rm(part_emission_table,part_emission_table_withname)}
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_partemission_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Results/tfs_part_load_emissions.csv");
    temp_partemission <- read_csv(temp_partemission_fn, col_types = cols()) %>%
      rename(`Participated Load` = participatedload,
             `Storage Loss` = storageloss) %>%
      mutate(case = cases[i],
             year = years[j])
    if (!exists('part_emission_table')){
      part_emission_table <- temp_partemission
      rm(temp_partemission,temp_partemission_fn)
    } else {
      part_emission_table <- rbind(part_emission_table, temp_partemission)
      rm(temp_partemission,temp_partemission_fn)
    }
  }
}
part_emission_table_withname <- part_emission_table %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(part_emission_table_withname, paste0(RunFdr,"/CompiledResults/tfs_part_emissions.csv"))
