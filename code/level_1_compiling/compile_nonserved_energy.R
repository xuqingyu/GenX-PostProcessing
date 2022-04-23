# compile nse
# Combining nse result----
if (exists('combined_nse')){
  rm('combined_nse');
}
print('begin compiling non-served energy')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){

    temp_nse_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Results/nse.csv");
    if (file.exists(temp_nse_fn)){
      # The top 3 rows of the file are Segment, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_nse = t(read_csv(temp_nse_fn, 
                               col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_nse) <- temp_nse[1,] # make the row one as column name
      temp_nse <- as_tibble(temp_nse[-c(1, dim(temp_nse)[1]),]) 
      # Remove the first row (as it as been set as column names)
      temp_nse$case = cases[i];
      temp_nse$year = years[j]
    }
    if(!exists('combined_nse')) {
      combined_nse <- temp_nse;
    } else {
      combined_nse <- rbind(combined_nse, temp_nse);
    }
  }
}
if(exists('combined_nse')){
  # combined_nse$AnnualSum <- as.numeric(combined_nse$AnnualSum);
  combined_nse <- combined_nse %>%
    mutate(AnnualSum = as.numeric(AnnualSum),
           Segment = as.numeric(Segment),
           Zone = as.character(as.numeric(Zone))) %>%
    left_join(zone_mapping,
              by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Segment, Zone, 
           AnnualSum) %>%
    write_csv(paste0(RunFdr,"/CompiledResults/nse.csv"));
  rm(temp_nse_fn, temp_nse, combined_nse)
  print('finished compiling non-served energy')
  print(Sys.time())
} else {
  print('there are no nse.csv files')
  print(Sys.time())
}
