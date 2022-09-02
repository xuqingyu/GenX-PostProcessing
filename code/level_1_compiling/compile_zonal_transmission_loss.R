# compile zonal transmission loss
# Combining zonaltransloss result----
if (exists('combined_ztloss')){
  rm('combined_ztloss');
}
print('begin compiling zonal transmission loss')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    
    temp_ztloss_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                          years[j],"_",cases[i],"/Results/zonaltransmissionlosses.csv");
    if (file.exists(temp_ztloss_fn)){
      # The top 3 rows of the file are Segment, Zone, and Sum, 
      # and the most left columns are names, take the transpose
      temp_ztloss = t(read_csv(temp_ztloss_fn, 
                            col_names = F, n_max = 2, col_types = cols()));
      colnames(temp_ztloss) <- temp_ztloss[1,] # make the row one as column name
      temp_ztloss <- as_tibble(temp_ztloss[-c(1, dim(temp_ztloss)[1]),]) 
      # Remove the first row (as it as been set as column names)
      temp_ztloss$case = cases[i];
      temp_ztloss$year = years[j]
    }
    if(!exists('combined_ztloss')) {
      combined_ztloss <- temp_ztloss;
    } else {
      combined_ztloss <- rbind(combined_ztloss, temp_ztloss);
    }
  }
}
if(exists('combined_ztloss')){
  combined_ztloss <- combined_ztloss %>%
    mutate(AnnualSum = as.numeric(AnnualSum),
           Zone = as.character(as.numeric(Zone))) %>%
    left_join(zone_mapping,
              by = c('Zone' = 'zone')) %>%
    rename(Region = region) %>%
    select(case, year, Region, Zone, 
           AnnualSum) %>%
    write_csv(paste0(RunFdr,"/CompiledResults/zonaltransmissionloss.csv"));
  rm(temp_ztloss_fn, temp_ztloss, combined_ztloss)
  print('finished compiling zonal transmission loss')
  print(Sys.time())
} else {
  print('there are zonal transmission loss')
  print(Sys.time())
}
