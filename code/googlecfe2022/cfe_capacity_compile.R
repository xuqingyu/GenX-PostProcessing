
if (exists('capacity')){
  rm('capacity');
}
print('begin compiling capacity')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],
                                "/Inputs/Generators_data.csv");
    temp_capacity_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                               years[j],"_",cases[i],"/Results/capacity.csv");
    if (file.exists(temp_capacity_fn)){
      temp_generator <- read_csv(temp_generator_fn, col_types = cols())
      temp_capacity_all <- read_csv(temp_capacity_fn, col_types = cols())
      for (k in 1:n_tfs){
        policy_column = which(colnames(temp_generator) ==paste0('RPSH_',k))
        cfe_rows = which(temp_generator[,policy_column] == 1)
        temp_capacity <- temp_capacity_all[cfe_rows, ] %>%
          mutate(Policy = k, case = cases[i], year = years[j])
        if(!exists('capacity')) {
          capacity <- temp_capacity;
          rm(temp_capacity, cfe_rows,policy_column)
        } else {
          capacity <- rbind(capacity, temp_capacity)
          rm(temp_capacity, cfe_rows,policy_column)
        }
      }
    }
    rm(temp_generator, temp_capacity_all,temp_generator_fn,temp_capacity_fn)
  }
}

if (exists('capacity')){
  gen_capacity <- capacity %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone)) %>%
    left_join(zone_mapping, by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Resource, Zone, Region, Policy,
           EndCap, EndEnergyCap, EndChargeCap, NewCap, RetCap, 
           NewEnergyCap, RetEnergyCap, NewChargeCap, RetChargeCap) %>%
    left_join(resource_mapping) %>%
    filter(!(Fuel %in% flexiload_list)) %>%
    na.omit()# filter out the "resource" that are not going to show
  
  temp_total_title <- subreg
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == subreg]
  
  gen_capacity_subregion <- gen_capacity %>%
    filter(Region %in% temp_total) %>%
    group_by(case, year, Fuel, Policy) %>%
    summarize( Capacity = sum(EndCap),
               `Charging Capacity` = sum(EndChargeCap),
               `Energy Capacity` = sum(EndEnergyCap),
               `Capacity Expansion` = sum(NewCap),
               `Energy Capacity Expansion` = sum(NewEnergyCap),
               `Charging Capacity Expansion` = sum(NewChargeCap),
               `Capacity Retirement` = sum(RetCap),
               `Energy Capacity Retirement` = sum(RetEnergyCap),
               `Charging Capacity Retirement` = sum(RetChargeCap)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case, year, Fuel, Scenario, TechSensitivity, Policy,
           Capacity, `Charging Capacity`, `Energy Capacity`,
           `Capacity Expansion`, `Energy Capacity Expansion`, `Charging Capacity Expansion`, 
           `Capacity Retirement`, `Energy Capacity Retirement`, `Charging Capacity Retirement`)
  write_csv(gen_capacity_subregion,paste0(RunFdr,'/CompiledResults/',subreg,
                                          '/Generation/tfs_gen_capacity_',temp_total_title,".csv"))
  print('finished compiling capacity')
  print(Sys.time())
  rm(gen_capacity,temp_total_title,temp_total,gen_capacity_subregion,capacity)
} else {
  print('there is no capacity file in any folder')
  print(Sys.time())
}


