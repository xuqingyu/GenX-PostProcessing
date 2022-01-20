# This file remaps the reduced time-series of each zone into the original 8760 ts.

# source('./code/Header.R')
if (exists('energyprice_ts')){
  rm(energyprice_ts,energyprice_reduced)
}
k = 1
for (i in 1:length(TS_cases)){
  for (j in length(years)){# only the last year is calculated on purpose
    temp_price_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Results/prices.csv");
    temp_load_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/Load_data.csv");
    temp_ts_mapping_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/Period_map.csv");
    if (file.exists(temp_price_fn)){
      
      temp_price_ts = read_csv(temp_price_fn, col_types = cols()) %>% 
        rename(Time_Index = Zone) %>% 
        mutate(Time_Index = as.numeric(str_remove(Time_Index,'t'))) %>% 
        pivot_longer(cols = !c(Time_Index), names_to = 'Zone', values_to = 'Price') %>%
        mutate(Zone = as.character(round(as.numeric(Zone),0))) %>%
        left_join(zone_mapping, by= c('Zone' = 'zone')) %>%
        rename(Region = region)
      
      temp_price_ts$case <- TS_cases[i]
      temp_price_ts$year <- years[j]
      
      if (file.exists(temp_ts_mapping_fn)){
        ts_mapping <- read_csv(temp_ts_mapping_fn, col_types = cols())
        n_slot <- dim(ts_mapping)[1]
        Hours_per_period <- read_csv(temp_load_fn, col_types = cols())$Timesteps_per_Rep_Period %>% na.omit()
        n_period <- read_csv(temp_load_fn,col_types = cols())$Rep_Periods %>% na.omit()
        model_hours <- Hours_per_period*n_slot;
        HourID = c(1:model_hours)
        Slot <- rep(ts_mapping$Period_Index,each = Hours_per_period)
        template <- as_tibble(cbind(HourID, Slot)) %>% 
          left_join(ts_mapping, by = c('Slot' = 'Period_Index')) %>% 
          mutate(Time_Index = rep(c(1:Hours_per_period),times = n_slot)) %>%
          mutate(Time_Index = Time_Index + Hours_per_period*(Rep_Period-1))
        temp_price_ts_full <- left_join(template, temp_price_ts, by = c('Time_Index')) %>%
          select(HourID, Zone, Price, Region, case,year)
      } else {
        temp_price_ts_full <- temp_price_ts %>%
          rename(HourID = Time_Index)
      }
    }
    if(!exists('energyprice_ts')){
      energyprice_ts <- temp_price_ts_full;
      energyprice_reduced <- temp_price_ts;
    } else {
      energyprice_ts <- rbind(energyprice_ts, temp_price_ts_full);
      energyprice_reduced <- rbind(energyprice_reduced, temp_price_ts)
    }
  }
  print(i)
  if ((i %% 10 == 0)| (i == length(TS_cases))) {
    write_csv(energyprice_ts,paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries_',k,'.csv'))
    write_csv(energyprice_reduced,paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries_reduced_',k,'.csv'))
    rm(energyprice_ts,temp_price_ts,energyprice_reduced,temp_price_ts_full)
    k = k +1
    print(k)
  }
}
maxk = k - 1
k = 1
for (k in (1:maxk)){
  temp_energyprice_ts <- read_csv(paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries_',k,'.csv'),col_types = cols())
  if(!exists('energyprice_ts')){
    energyprice_ts <- temp_energyprice_ts;
  } else {
    energyprice_ts <- rbind(energyprice_ts, temp_energyprice_ts);
  }
  print(k)
}
write_csv(energyprice_ts,paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries.csv'))
energyprice_ts_statistics <- energyprice_ts %>%
  group_by(Zone, Region, case, year) %>%
  summarize(`Price Mean` = mean(Price),
            `Price Std.` = sd(Price),
            `25th Quantile` = quantile(Price, 0.25),
            `Median` = quantile(Price, 0.50),
            `75th Quantile` = quantile(Price, .75))
write_csv(energyprice_ts_statistics,paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries_statistics.csv'))
rm(temp_energyprice_ts, energyprice_ts, energyprice_ts_statistics)

