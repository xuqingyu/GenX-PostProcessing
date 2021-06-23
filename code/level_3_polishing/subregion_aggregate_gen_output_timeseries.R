source('./code/Header.R')
if (exists('injection_timeseries')){
  rm(injection_timeseries)
}
for ( k in 1:n_subregions){
  temp_total_title <- Subregions[k]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[k]]
  for (i in 1:length(TS_cases)){
    for (j in 1:length(years)){
      temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/Generators_data.csv");
      temp_power_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Results/power.csv");
      temp_charge_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Results/charge.csv");
      temp_load_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/Load_data.csv");
      temp_ts_mapping_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/time_series_mapping.csv");
      if (file.exists(temp_power_fn)){
        temp_generator <- read_csv(temp_generator_fn);
        # The top 3 rows of the file are Resource, Zone, and Sum (to drop), and the most left columns are names, take the transpose
        temp_power_ts = t(read.csv(temp_power_fn,header = F)[-3,]); 
        # make the row one as column name
        powercolnames <- temp_power_ts[1,];
        # Remove the first row (as it as been set as column names), and the last row
        temp_power_ts <- temp_power_ts[-c(1,length(temp_power_ts[,1])),];
        colnames(temp_power_ts) <- powercolnames;
        temp_power_ts <- as_tibble(temp_power_ts);
        temp_power_ts <- cbind(temp_power_ts,select(temp_generator,region, cluster,Fuel)) %>%
          rename(Region = region,  Cluster = cluster) %>%
          filter(Region %in% temp_total);
        zcf_row <- which(temp_power_ts$Fuel=='ZCF');
        temp_power_ts$Resource[zcf_row] <- paste(temp_power_ts$Resource[zcf_row],'_ZCF',sep='')
        temp_power_ts <- temp_power_ts %>% 
          select(-Fuel) %>% 
          left_join(resource_mapping_includingflexibleload,by=c('Resource' = 'All_Resource')) %>%
          rename(Fuel = `All_Fuel`) %>%
          pivot_longer(cols = !c(Region, Zone, Cluster, Resource, Fuel),names_to = 'Time_index') %>%
          mutate(`Time_index` = as.numeric(str_remove(`Time_index`, 't')), value = as.numeric(value)) %>%
          group_by(Fuel,`Time_index`) %>%
          summarize(Injection = round(sum(value),3))
        
        temp_charge_ts = t(read.csv(temp_charge_fn,header = F)[-3,]); 
        # make the row one as column name
        powercolnames <- temp_charge_ts[1,];
        # Remove the first row (as it as been set as column names), and the last row
        temp_charge_ts <- temp_charge_ts[-c(1,length(temp_charge_ts[,1])),];
        colnames(temp_charge_ts) <- powercolnames;
        temp_charge_ts <- as_tibble(temp_charge_ts);
        temp_charge_ts <- cbind(temp_charge_ts,select(temp_generator,region, cluster,Fuel)) %>%
          rename(Region = region,  Cluster = cluster) %>%
          filter(Region %in% temp_total);
        zcf_row <- which(temp_charge_ts$Fuel=='ZCF');
        temp_charge_ts$Resource[zcf_row] <- paste(temp_charge_ts$Resource[zcf_row],'_ZCF',sep='')
        temp_charge_ts <- temp_charge_ts %>% 
          select(-Fuel) %>% 
          left_join(resource_mapping_includingflexibleload,by=c('Resource' = 'All_Resource')) %>%
          rename(Fuel = `All_Fuel`) %>%
          pivot_longer(cols = !c(Region, Zone, Cluster, Resource, Fuel),names_to = 'Time_index') %>%
          mutate(`Time_index` = as.numeric(str_remove(`Time_index`, 't')), value = as.numeric(value)) %>%
          group_by(Fuel,`Time_index`) %>%
          summarize(Withdrawal = round(sum(value),3))
        temp_injection_ts <- left_join(temp_charge_ts, temp_power_ts) %>%
          mutate(Injection = Injection - Withdrawal) %>%
          select(-Withdrawal)
        rm(temp_charge_ts, temp_power_ts)
        flexload_row <- which(temp_injection_ts$Fuel == 'Flexible Load');
        temp_injection_ts$Injection[flexload_row] <- (-1)*temp_injection_ts$Injection[flexload_row]
        temp_injection_ts$case <- TS_cases[i]
        temp_injection_ts$year <- years[j]
        
        if (file.exists(temp_ts_mapping_fn)){
          ts_mapping <- read_csv(temp_ts_mapping_fn)
          n_slot <- dim(ts_mapping)[1]
          Hours_per_period <- read_csv(temp_load_fn)$Hours_per_period %>% na.omit()
          n_period <- read_csv(temp_load_fn)$Subperiods %>% na.omit()
          model_hours <- Hours_per_period*n_slot;
          HourID = c(1:model_hours)
          Slot <- rep(ts_mapping$slot,each = Hours_per_period)
          template <- as_tibble(cbind(HourID, Slot)) %>% 
            left_join(ts_mapping, by = c('Slot' = 'slot')) %>% 
            mutate(Time_index = rep(c(1:Hours_per_period),times = n_slot)) %>%
            mutate(Time_index = Time_index + Hours_per_period*(cluster-1))
          temp_injection_ts <- left_join(template, temp_injection_ts, by = c('Time_index')) %>%
            select(Fuel, HourID, Injection, case,year)
        } else {
          temp_injection_ts <- temp_injection_ts %>%
            rename(HourID = Time_index)
        }
      }
      if(!exists('injection_timeseries')){
        injection_timeseries <- temp_injection_ts;
      } else {
        injection_timeseries <- rbind(injection_timeseries, temp_injection_ts);
      }
    }
  }
  write_csv(injection_timeseries,paste0(RunFdr,'/CompiledResults/',Subregions[k],'/Generation/Gen_fullyear_timeseries_',temp_total_title,".csv"))
  rm(injection_timeseries)
}


# temp <- read_csv(paste0(RunFdr,'/CompiledResults/',Subregions[1],'/Generation/Gen_fullyear_timeseries_PJM.csv')) %>%
#   filter(case == 'deepdecarbonization_mid', year == 2050,Fuel != 'Flexible Load') %>%
#   mutate(Fuel = factor(Fuel,levels = capacity_resource_levels))
# temp_plot <- temp %>% filter(HourID %in% c(1:672))
# ggplot(data=temp_plot,aes(x=HourID, y=Injection, fill=Fuel)) +
#   geom_area()+
#   scale_fill_manual(name = "Resources", values = fuel_colors)
