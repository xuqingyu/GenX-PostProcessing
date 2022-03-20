
# Curtailment ----

if (exists('combined_recurtail')){
  rm('combined_recurtail');
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
      
      temp_recurtail_all = t(read_csv(temp_curtail_fn, 
                                  col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_recurtail_all) <- temp_recurtail_all[1,] 
      # make the row one as column name
      temp_recurtail_all <- as_tibble(temp_recurtail_all[-c(1, dim(temp_recurtail_all)[1]),]) 
      # Remove the first row (as it as been set as column names)      
      
      temp_generator <- read_csv(temp_generator_fn, col_types = cols());
      for (k in 1:n_tfs){
        policy_column = which(colnames(temp_generator) == paste0('RPSH_',k))
        cfe_rows = which(temp_generator[,policy_column] == 1)
        temp_recurtail <- temp_recurtail_all[cfe_rows, ] %>%
          mutate(Policy = k, case = cases[i], year = years[j])
        if(!exists('combined_recurtail')) {
          combined_recurtail <- temp_recurtail;
          rm(temp_recurtail, cfe_rows, policy_column)
        } else {
          combined_recurtail <- rbind(combined_recurtail, temp_recurtail)
          rm(temp_recurtail, cfe_rows, policy_column)
        }
      }
    }
  }
}
if (exists('combined_recurtail')){
  temp_total_title <- subreg
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == subreg]
  gen_curtail <- combined_recurtail %>%
    mutate(AnnualSum = as.numeric(as.character(AnnualSum)),
           Zone = factor(Zone, levels = zone_mapping$zone)) %>%
    left_join(zone_mapping, by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Resource, Zone, Policy, AnnualSum) %>%
    left_join(resource_mapping) %>%
    filter(!(Fuel %in% storage_fuel),
           grepl('Wind|Solar', Fuel)) %>%
    na.omit()

  gencurtail_subregion <- gen_curtail%>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel, Policy) %>%
    summarize(AnnualCurtail = sum(AnnualSum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Fuel, Scenario, `TechSensitivity`, Policy, AnnualCurtail)
  write_csv(gencurtail_subregion,paste0(RunFdr,'/CompiledResults/',subreg,
                                        '/Generation/tfs_gen_curtailment_',temp_total_title,".csv"))
  
  print('finished compiling curtailment')
  print(Sys.time())
  rm(gen_curtail, temp_total_title, temp_total,gencurtail_subregion)
} else {
  print('there are no curtail.csv files')
  print(Sys.time())
}
