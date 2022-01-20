# CFE study capacity and output
# Capacity and output
# Combining capacity data----



if (exists('capacity')){
  rm('capacity');
}
print('begin compiling capacity')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],
                                "/Inputs/Generators_data.csv");
    temp_capacity_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                               years[j],"_",cases[i],"/Results/capacity.csv");
    if (file.exists(temp_capacity_fn)){
      temp_generator <- read_csv(temp_generator_fn, col_types = cols());
      cfe_rows = which(temp_generator$RPSH_1 == 1)
      temp_capacity <- read_csv(temp_capacity_fn, col_types = cols()) %>%
        filter(Region != 'n/a') %>% # remove the total row;
        mutate(case = cases[i], year = years[j]); 
      temp_capacity = temp_capacity[cfe_rows, ]
    }
    if(!exists('capacity')) {
      capacity <- temp_capacity;
    } else {
      capacity <- rbind(capacity, temp_capacity);
    }
  }
}

if (exists('capacity')){
  capacity$Zone <-as.numeric(capacity$Zone);
  capacity$Cluster <-as.numeric(capacity$Cluster);
  
  # set up the first year data
  temp_capacity <- subset(capacity,year == years[1]) %>%
    mutate(EndCap = StartCap,
           EndEnergyCap = StartEnergyCap,
           EndChargeCap = StartChargeCap,
           NewCap = 0,
           RetCap = 0,
           NewEnergyCap = 0,
           RetEnergyCap = 0,
           NewChargeCap = 0,
           RetChargeCap = 0,
           year = settings$start_year[1]);
  capacity <- rbind(temp_capacity, capacity);
  capacity <- select(capacity, case, year, Region, Resource, Zone, Cluster, 
                     EndCap, EndEnergyCap, EndChargeCap, NewCap, RetCap, 
                     NewEnergyCap, RetEnergyCap, NewChargeCap, RetChargeCap)

  rm(temp_capacity, temp_generator,temp_capacity_fn,temp_generator_fn)
  
  gen_capacity = capacity %>%
    left_join(resource_mapping) %>%
    na.omit()# filter out the "resource" that are not going to show
  

  temp_total_title <- subreg
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == subreg]
  
  gen_capacity_subregion <- gen_capacity %>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel) %>%
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
    select(case, year, Fuel, Scenario, TechSensitivity, 
           Capacity, `Charging Capacity`, `Energy Capacity`,
           `Capacity Expansion`, `Energy Capacity Expansion`, `Charging Capacity Expansion`, 
           `Capacity Retirement`, `Energy Capacity Retirement`, `Charging Capacity Retirement`)
  # the modification below is added because there is no incentive for 24/7 consumers to purchase storage under 100% annual matching case
  nostorage_row = which(gen_capacity_subregion$Fuel=='Battery' & grepl('nocip|annual100',gen_capacity_subregion$case))
  gen_capacity_subregion$Capacity[nostorage_row] = 0
  gen_capacity_subregion$`Energy Capacity`[nostorage_row] = 0
  gen_capacity_subregion$`Capacity Expansion`[nostorage_row] = 0
  gen_capacity_subregion$`Energy Capacity Expansion`[nostorage_row] = 0
  #
  write_csv(gen_capacity_subregion,paste0(temp_RunFdr,'/CompiledResults/',subreg,
                                          '/Generation/CFE_Gen_Capacity_',temp_total_title,".csv"))
  print('finished compiling capacity')
  print(Sys.time())
} else {
  print('there is no capacity file in any folder')
  print(Sys.time())
}


# Output

if (exists('combined_power')){
  rm('combined_power');
}
print('begin compiling energy generation')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],
                                "/Inputs/Generators_data.csv");
    temp_power_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                            "_",cases[i],"/Results/power.csv");
    if (file.exists(temp_power_fn)){
      # The top 3 rows of the file are Resource, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_power = t(read_csv(temp_power_fn, 
                              col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_power) <- temp_power[1,] # make the row one as column name
      temp_power <- as_tibble(temp_power[-c(1, dim(temp_power)[1]),]) 
      # Remove the first row (as it as been set as column names)
      
      temp_generator <- read_csv(temp_generator_fn, col_types = cols());
      cfe_rows = which(temp_generator$RPSH_1 == 1)
      temp_power <- cbind(temp_power,temp_generator$cluster);
      colnames(temp_power)[dim(temp_power)[2]] <- "Cluster";
      temp_power$case = cases[i];
      temp_power$year = years[j]
      temp_power = temp_power[cfe_rows, ]
    }
    if(!exists('combined_power')) {
      combined_power <- temp_power;
    } else {
      combined_power <- rbind(combined_power, temp_power);
    }
  }
}
if (exists('combined_power')){
  combined_power$Sum <- as.numeric(combined_power$Sum);
  combined_power <- left_join(combined_power, zone_mapping, 
                              by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Resource, Zone, Cluster, Sum);
  
  rm(temp_generator_fn, temp_power_fn, temp_power, temp_generator)
  
  gen_power <- combined_power %>%
    left_join(resource_mapping) %>%
    filter(!(Fuel %in% storage_fuel)) %>%
    na.omit()

  temp_total_title <- subreg
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == subreg]
  genoutput_subregion <- gen_power%>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel) %>%
    summarize(AnnualOutput = sum(Sum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Fuel, Scenario, `TechSensitivity`,
           AnnualOutput)
  write_csv(genoutput_subregion,paste0(temp_RunFdr,'/CompiledResults/',subreg,
                                       '/Generation/CFE_Gen_Output_',temp_total_title,".csv"))

  print('finished compiling energy generation')
  print(Sys.time())
} else {
  print('there are no power.csv files')
  print(Sys.time())
}

# Curtailment ----

if (exists('combined_REcurtail')){
  rm('combined_REcurtail');
}
print('begin compiling curtailment')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],
                                "/Inputs/Generators_data.csv");
    temp_curtail_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                              "_",cases[i],"/Results/curtail.csv");
    if (file.exists(temp_curtail_fn)){

      temp_REcurtail = t(read_csv(temp_curtail_fn, 
                                  col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_REcurtail) <- temp_REcurtail[1,] 
      # make the row one as column name
      temp_REcurtail <- as_tibble(temp_REcurtail[-c(1, dim(temp_REcurtail)[1]),]) 
      # Remove the first row (as it as been set as column names)      
      
      temp_generator <- read_csv(temp_generator_fn, col_types = cols());
      cfe_rows = which(temp_generator$RPSH_1 == 1)
      temp_REcurtail$case = cases[i];
      temp_REcurtail$year = years[j]
      temp_REcurtail = temp_REcurtail[cfe_rows,]
    }
    if(!exists('combined_REcurtail')) {
      combined_REcurtail <- temp_REcurtail;
    } else {
      combined_REcurtail <- rbind(combined_REcurtail, temp_REcurtail);
    }
  }
}
if (exists('combined_REcurtail')){
  combined_REcurtail$Sum <- as.numeric(combined_REcurtail$Sum);
  combined_REcurtail <- combined_REcurtail %>%
    group_by(case,year,Zone,Resource) %>%
    summarize(Sum = sum(Sum));
  
  combined_REcurtail <- left_join(combined_REcurtail, zone_mapping, 
                                  by = c('Zone' = 'zone'));
  rm(temp_generator_fn, temp_curtail_fn,temp_REcurtail,
     temp_generator)
  gen_curtail <- combined_REcurtail %>%
    left_join(resource_mapping) %>%
    filter(!(Fuel %in% storage_fuel),
           grepl('Wind|Solar', Fuel)) %>%
    na.omit()

  temp_total_title <- subreg
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == subreg]
  gencurtail_subregion <- gen_curtail%>%
    filter(region %in% temp_total) %>%
    group_by(case,year,Fuel) %>%
    summarize(AnnualCurtail = sum(Sum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Fuel, Scenario, `TechSensitivity`, AnnualCurtail)
  write_csv(gencurtail_subregion,paste0(temp_RunFdr,'/CompiledResults/',subreg,
                                       '/Generation/CFE_Gen_Curtailment_',temp_total_title,".csv"))

  print('finished compiling curtailment')
  print(Sys.time())
} else {
  print('there are no curtail.csv files')
  print(Sys.time())
}

