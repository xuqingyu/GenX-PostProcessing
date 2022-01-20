#--------------------------------------#
# Combining NSE cost -----
#--------------------------------------#
if (exists('ZonalNSECost')){
  rm('ZonalNSECost')
}
print('begin compiling NSE cost')
print(Sys.time())
for ( i in 1:length(cases)) {
  for (j in 1:length(years)) {
    # '[...]/[Running_folder]/[year]/[case_id]_[year]_[case_description]/Results/'
    temp <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/costs.csv");
    if (file.exists(temp)) {
      temp_cost <- read_csv(temp, col_types = cols()) %>%
        select(-Total) %>%
        filter(Costs %in% c("cNSE")); 
      NoZone <- dim(temp_cost)[2]-1;
      colnames(temp_cost) <- c("item",c(1:NoZone));
      temp_cost <- temp_cost %>%
        pivot_longer(c(2:(NoZone+1)),names_to = 'zone',values_to = 'value') %>%
        mutate(case = cases[i],year = years[j]) %>%
        select(zone,case,year,item,value) %>%
        mutate(zone = as.numeric(zone), 
               value = as.numeric(value));
      if (!exists('ZonalNSECost')) {
        ZonalNSECost <- temp_cost
      } else {
        ZonalNSECost <- rbind(ZonalNSECost, temp_cost)
      }
    }
  }
}
if (exists('ZonalNSECost')){
  write_csv(ZonalNSECost, paste0(RunFdr,"/CompiledResults/LSENSECost.csv"))
  rm(temp,temp_cost,NoZone)
  print('finished compiling NSE cost')
  print(Sys.time())
} else {
  print('there is no costs.csv')
  print(Sys.time())
}

