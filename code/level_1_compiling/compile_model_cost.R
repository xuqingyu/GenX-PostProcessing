#----------------------------#
#     Combining Cost data ----
#----------------------------#
if (exists('costs')){
  rm('costs')
}
print('begin compiling model cost data')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_costs_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/costs.csv");
    if (file.exists(temp_costs_fn)){
      temp_costs = read_csv(temp_costs_fn) %>%
        mutate(case = cases[i], year = years[j])
      if(!exists('costs')) {
        costs <- temp_costs;
      } else {
        costs <- rbind(costs, temp_costs);
      }
    }
  }
}
if (exists('costs')) {
  write_csv(costs, paste0(RunFdr,"/CompiledResults/costs.csv"));
  rm(temp_costs_fn, temp_costs)
  print('finished compiling model cost data')
} else {
  print('there are no costs.csv')
}
