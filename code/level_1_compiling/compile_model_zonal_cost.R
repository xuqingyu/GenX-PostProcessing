# ---------------------------##
# Compile Zonal Cost ----
# ---------------------------##
if (exists('ZonalCost')){
  rm('ZonalCost')
}
print('begin compiling zonal cost')
for ( i in 1:length(cases)) {
  for (j in 1:length(years)) {
    # '[...]/[Running_folder]/[year]/[case_id]_[year]_[case_description]/Results/'
    temp <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/costs.csv");
    if (file.exists(temp))
    {
      temp_cost <- read_csv(temp) %>%
        select(-Total) %>%
        filter(Costs %in% c("cFix","cVar","cNSE","cStart","cCO2Tax","cCO2Capture")); 
      NoZone <- dim(temp_cost)[2]-1;
      colnames(temp_cost) <- c("item",c(1:NoZone));
      temp_cost <- temp_cost %>%
        pivot_longer(c(2:(NoZone+1)),names_to = 'zone',values_to = 'value') %>%
        mutate(case = cases[i],year = years[j]) %>%
        select(zone,case,year,item,value) %>%
        mutate(zone <- as.numeric(zone), value <- as.numeric(value));
      if (!exists('ZonalCost')) {
        ZonalCost <- temp_cost
      } else {
        ZonalCost <- rbind(ZonalCost, temp_cost)
      }
    }
  }
}
if (exists('ZonalCost')) {
  write_csv(ZonalCost, paste0(RunFdr,"/CompiledResults/Zonalcost.csv"))
  print('finished compiling zonal cost')
} else {
  print('there are no costs.csv')
}
