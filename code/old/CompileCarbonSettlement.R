
source("./code/Header.R")
# Compile Zonal Cost -------------------------------------------------------------
for ( i in 1:length(cases))
{
  for (j in 1:length(years))
  {
    # '[...]/[Running_folder]/[year]/[case_id]_[year]_[case_description]/Results/'
    temp <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/costs.csv");
    if (file.exists(temp))
    {
      
      temp_emisisonsettlement <- read_csv(temp) %>%
        select(-Total) %>%
        filter(Costs %in% c("cEmissionsRevenue","cEmissionsCost")); 
      NoZone <- dim(temp_emisisonsettlement)[2]-1;
      colnames(temp_emisisonsettlement) <- c("item",c(1:NoZone));
      temp_emisisonsettlement <- temp_emisisonsettlement %>%
        pivot_longer(c(2:(NoZone+1)),names_to = 'zone',values_to = 'value') %>%
        mutate(case = cases[i],year = years[j]) %>%
        select(zone,case,year,item,value);
      temp_emisisonsettlement$zone <- as.numeric(temp_emisisonsettlement$zone);
      temp_emisisonsettlement$value <- (-1)*as.numeric(temp_emisisonsettlement$value);
      if (!exists('ZonalEmisisonSettlement'))
      {
        ZonalEmisisonSettlement <- temp_emisisonsettlement
      }
      else
      {
        ZonalEmisisonSettlement <- rbind(ZonalEmisisonSettlement, temp_emisisonsettlement)
      }
    }
  }
}

write_csv(ZonalEmisisonSettlement, paste0(RunFdr,"/CompiledResults/ZonalEmisisonSettlement.csv"))