
# Combining Curtailment result----
if (exists('combined_REcurtail')){
  rm('combined_REcurtail');
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
      # temp_REcurtail = t(read.csv(temp_curtail_fn,header = F)[1:3,]);
      # powercolnames <- temp_REcurtail[1,];
      # temp_REcurtail <- temp_REcurtail[-c(1,length(temp_REcurtail[,1])),];
      # colnames(temp_REcurtail) <- powercolnames;
      # temp_REcurtail <- as.data.frame(temp_REcurtail);
      
      # The top 3 rows of the file are Resource, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_REcurtail = t(read_csv(temp_curtail_fn, 
                               col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_REcurtail) <- temp_REcurtail[1,] 
      # make the row one as column name
      temp_REcurtail <- as_tibble(temp_REcurtail[-c(1, dim(temp_REcurtail)[1]),]) 
      # Remove the first row (as it as been set as column names)      
      
      temp_generator <- read_csv(temp_generator_fn, col_types = cols());
      temp_REcurtail <- cbind(temp_REcurtail,temp_generator$Fuel);
      colnames(temp_REcurtail)[dim(temp_REcurtail)[2]] <- "Fuel";
      temp_REcurtail$case = cases[i];
      temp_REcurtail$year = years[j]
    }
    if(!exists('combined_REcurtail')) {
      combined_REcurtail <- temp_REcurtail;
    } else {
      combined_REcurtail <- rbind(combined_REcurtail, temp_REcurtail);
    }
  }
}
if (exists('combined_REcurtail')){
  combined_REcurtail$AnnualSum <- as.numeric(combined_REcurtail$AnnualSum);
  # combined_REcurtail <- combined_REcurtail %>%
  #   group_by(case,year,Zone,Resource,Fuel) %>%
  #   summarize(Sum = sum(Sum));
  # 
  combined_REcurtail <- left_join(combined_REcurtail, zone_mapping, 
                                  by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Resource, Zone, Fuel, AnnualSum);
  # if (identical(years, c(2030, 2040, 2050))){
  #   combined_REcurtail_temp1 <- subset(combined_REcurtail,Fuel == "ZCF") %>%
  #     mutate(Resource = paste(Resource,"_ZCF",sep = ""));
  #   combined_REcurtail <- rbind(combined_REcurtail_temp1, 
  #                               subset(combined_REcurtail, Fuel != "ZCF"));
  #   rm(combined_REcurtail_temp1)
  # }
  combined_REcurtail <- subset(combined_REcurtail,select = -c(Fuel));
  
  write_csv(combined_REcurtail, 
            paste0(RunFdr,"/CompiledResults/renewablecurtail.csv"));
  rm(temp_generator_fn, temp_curtail_fn,temp_REcurtail,
     temp_generator,combined_REcurtail)
  print('finished compiling curtailment')
  print(Sys.time())
} else {
  print('there are no curtail.csv files')
  print(Sys.time())
}

