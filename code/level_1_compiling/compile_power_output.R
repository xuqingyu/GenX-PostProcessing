# Combining Power result----
if (exists('combined_power')){
  rm('combined_power');
}
print('begin compiling energy generation')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_power_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/power.csv");
    if (file.exists(temp_power_fn)){
      # The top 3 rows of the file are Resource, Zone, and Sum, and the most left columns are names, take the transpose
      temp_power = t(read.csv(temp_power_fn,header = F)[1:3,]); 
      # make the row one as column name
      powercolnames <- temp_power[1,];
      # Remove the first row (as it as been set as column names), and the last row
      temp_power <- temp_power[-c(1,length(temp_power[,1])),];
      colnames(temp_power) <- powercolnames;
      temp_power <- as.data.frame(temp_power);
      temp_generator <- read_csv(temp_generator_fn);
      temp_power <- cbind(temp_power,temp_generator$Fuel);
      colnames(temp_power)[dim(temp_power)[2]] <- "Fuel";
      temp_power <- cbind(temp_power,temp_generator$cluster);
      colnames(temp_power)[dim(temp_power)[2]] <- "Cluster";
      temp_power$case = cases[i];
      temp_power$year = years[j]
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
  combined_power <- left_join(combined_power, zone_mapping, by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Resource, Zone, Cluster, Fuel, Sum);
  power_for_settlement <- combined_power;
  write_csv(power_for_settlement, paste0(RunFdr,"/CompiledResults/power_for_settlement.csv"));
  
  combined_power_temp1 <- subset(combined_power,Fuel == "ZCF") %>%
    mutate(Resource = paste(Resource,"_ZCF",sep = ""));
  combined_power <- rbind(combined_power_temp1, subset(combined_power,Fuel != "ZCF"));
  combined_power <- subset(combined_power,select = -c(Fuel));
  write_csv(combined_power, paste0(RunFdr,"/CompiledResults/power.csv"));
  rm(temp_generator_fn, temp_power_fn, temp_power,powercolnames,combined_power,power_for_settlement,combined_power_temp1)
  print('finished compiling energy generation')
} else {
  print('there are no power.csv files')
}


