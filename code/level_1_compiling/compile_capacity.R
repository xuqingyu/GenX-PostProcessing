# Combining capacity data----
if (exists('capacity')){
  rm('capacity','capacity_for_settlement','capacity_ZCF');
}
print('begin compiling capacity')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_capacity_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/capacity.csv");
    if (file.exists(temp_capacity_fn)){
      temp_capacity <- read_csv(temp_capacity_fn) %>%
        filter(Region != 'n/a') %>% # remove the total row;
        mutate(case = cases[i], year = years[j]); 
      temp_generator <- read_csv(temp_generator_fn);
      temp_capacity <- cbind(temp_capacity,temp_generator$Fuel);
      colnames(temp_capacity)[dim(temp_capacity)[2]] <- "Fuel";
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
  capacity <- select(capacity, case, year, Region, Resource, Zone, Cluster, Fuel, 
                     EndCap, EndEnergyCap, EndChargeCap, NewCap, RetCap, NewEnergyCap,
                     RetEnergyCap, NewChargeCap, RetChargeCap)
  capacity_for_settlement <- capacity;
  write_csv(capacity_for_settlement, paste0(RunFdr,"/CompiledResults/capacity_for_settlement.csv"));
  
  # change the ZCF setting
  capacity_ZCF <- subset(capacity,Fuel == "ZCF") %>%
    mutate(Resource = paste(Resource,"_ZCF",sep = ""));
  capacity <- rbind(capacity_ZCF, subset(capacity,Fuel != "ZCF"));
  capacity <- subset(capacity,select = -c(Fuel));
  write_csv(capacity, paste0(RunFdr,"/CompiledResults/capacity.csv"));
  
  rm(capacity, capacity_for_settlement, capacity_ZCF, temp_capacity, temp_generator,temp_capacity_fn,temp_generator_fn)
  print('finished compiling capacity')
} else {
  print('there is no capacity file in any folder')
}


