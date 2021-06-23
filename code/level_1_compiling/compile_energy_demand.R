
#Combining Demand data----

#note that load_data and time_weights must be in pairs
if (exists('demand')){
  rm('demand','total_load','total_demand_region');
}
print('begin compiling energy demand')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_load_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Load_data.csv");
    temp_timeweight_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/time_weights.csv");
    if (file.exists(temp_load_fn)) {
      temp_demand <- read_csv(temp_load_fn,progress = show_progress()) %>%
        select(-c(1:8)) %>% # the left 8 columns are for policy;
        mutate(case = cases[i], year = years[j]); 
      timeweight <- read_csv(temp_timeweight_fn);
      temp_demand$weight <- timeweight$Weight;
    }
    if(!exists('demand')){
      demand <- temp_demand;
    } else {
      demand <- rbind(demand, temp_demand);
    }
  }
}


if(exists('demand')){
  write_csv(demand, paste0(RunFdr,"/CompiledResults/demand.csv"))
  
  total_load <- demand %>%
    mutate(systemload = rowSums(demand[,2:(ncol(demand)-3)])) %>%
    group_by(case, year) %>%
    summarize(systemload = sum(systemload*weight));
  write_csv(total_load, paste0(RunFdr, "/CompiledResults/load_sums_weighted.csv"))
  
  total_demand_region <- demand %>%
    pivot_longer(cols = !c('Time_index','case','year','weight'),names_to = "Region",values_to = "Load_MW") %>%
    group_by(case,year,Region) %>%
    summarize(AnnualLoad = sum(Load_MW*weight)/1e6);
  write_csv(total_demand_region, paste0(RunFdr, "/CompiledResults/Total_load_summary.csv"))
  
  rm(temp_load_fn, temp_timeweight_fn, temp_demand,timeweight,demand,total_load,total_demand_region)
  print('finished compiling energy demand')
} else {
  print('there are no demand files in any folder')
}



