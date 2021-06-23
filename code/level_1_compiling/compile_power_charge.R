# Combining charge result----
if (exists('combined_charge')){
  rm('combined_charge');
}
print('begin compiling energy charge')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_charge_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/charge.csv");
    if (file.exists(temp_charge_fn)){
      # The top 3 rows of the file are Resource, Zone, and Sum, and the most left columns are names, take the transpose
      temp_charge = t(read.csv(temp_charge_fn,header = F)[1:3,]); 
      # make the row one as column name
      powercolnames <- temp_charge[1,];
      # Remove the first row (as it as been set as column names), and the last row
      temp_charge <- temp_charge[-c(1,length(temp_charge[,1])),];
      colnames(temp_charge) <- powercolnames;
      temp_charge <- as.data.frame(temp_charge);
      temp_generator <- read_csv(temp_generator_fn);
      temp_charge <- cbind(temp_charge,temp_generator$Fuel);
      colnames(temp_charge)[dim(temp_charge)[2]] <- "Fuel";
      temp_charge <- cbind(temp_charge,temp_generator$cluster);
      colnames(temp_charge)[dim(temp_charge)[2]] <- "Cluster";
      temp_charge$case = cases[i];
      temp_charge$year = years[j]
    }
    if(!exists('combined_charge')) {
      combined_charge <- temp_charge;
    } else {
      combined_charge <- rbind(combined_charge, temp_charge);
    }
  }
}
if(exists('combined_charge')){
  combined_charge$Sum <- as.numeric(combined_charge$Sum);
  combined_charge <- left_join(combined_charge, zone_mapping, by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Resource, Zone, Cluster, Fuel, Sum);
  charge_for_settlement <- combined_charge;
  write_csv(charge_for_settlement, paste0(RunFdr,"/CompiledResults/charge_for_settlement.csv"));
  
  combined_charge_temp1 <- subset(combined_charge,Fuel == "ZCF") %>%
    mutate(Resource = paste(Resource,"_ZCF",sep = ""));
  combined_charge <- rbind(combined_charge_temp1, subset(combined_charge,Fuel != "ZCF"));
  combined_charge <- subset(combined_charge,select = -c(Fuel));
  write_csv(combined_charge, paste0(RunFdr,"/CompiledResults/charge.csv"));
  rm(temp_generator_fn, temp_charge_fn, temp_charge,powercolnames,combined_charge,charge_for_settlement,combined_charge_temp1)
  print('finished compiling energy charge')
} else {
  print('there are no charge.csv files')
}