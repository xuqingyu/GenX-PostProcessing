# Plot load time-series
# source('./code/Header.R')
if (exists('load_timeseries')){
  rm(load_timeseries)
}
print('begin generating timeseries data')
for ( k in 1:n_subregions){
  temp_total_title <- Subregions[k]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[k]]
  injection_timeseries_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[k],
                                    '/Generation/Gen_fullyear_timeseries_',temp_total_title,'.csv');
  if (file.exists(injection_timeseries_fn)) {
    flexible_load_timeseries <- read_csv(injection_timeseries_fn, col_types = cols()) %>%
      filter(Fuel == 'Flexible Load') %>%
      select(-Fuel) %>%
      rename(`Flexible Demand` = Injection) %>%
      mutate(`Flexible Demand` = (-1)*`Flexible Demand`)
  } else {
    print('ERROR: there is no injection data')
  }
  
  for (i in 1:length(TS_cases)){
    for (j in 1:length(years)){
      temp_load_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],
                             "/Inputs/Load_data.csv");
      temp_ts_mapping_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],
                                   "/Inputs/time_series_mapping.csv");
      if (file.exists(temp_load_fn)){
        temp_demand <- read_csv(temp_load_fn, col_types = cols()) %>%
          select(-c(1:8)) %>%
          pivot_longer(cols=!c(Time_index),names_to = 'Zone', values_to = 'Load') %>%
          mutate(Zone = as.factor(str_remove(Zone, 'Load_MW_z'))) %>%
          left_join(zone_mapping, by = c('Zone' = 'zone')) %>%
          filter(region %in% temp_total) %>%
          group_by(Time_index) %>%
          summarize(Load = sum(Load)) %>%
          mutate(case = TS_cases[i],year = years[j] )
        
        if (file.exists(temp_ts_mapping_fn)){
          ts_mapping <- read_csv(temp_ts_mapping_fn, col_types = cols())
          n_slot <- dim(ts_mapping)[1]
          Hours_per_period <- read_csv(temp_load_fn, col_types = cols())$Hours_per_period %>% na.omit()
          n_period <- read_csv(temp_load_fn, col_types = cols())$Subperiods %>% na.omit()
          model_hours <- Hours_per_period*n_slot;
          HourID = c(1:model_hours)
          Slot <- rep(ts_mapping$slot,each = Hours_per_period)
          template <- as_tibble(cbind(HourID, Slot)) %>% 
            left_join(ts_mapping, by = c('Slot' = 'slot')) %>% 
            mutate(Time_index = rep(c(1:Hours_per_period),times = n_slot)) %>%
            mutate(Time_index = Time_index + Hours_per_period*(cluster-1))
          temp_load_ts <- left_join(template, temp_demand, by = c('Time_index')) %>%
            select(HourID, Load, case,year)
        } else {
          temp_load_ts <- temp_load_ts %>%
            rename(HourID = Time_index)
        }
        temp_flexible_demand <- flexible_load_timeseries %>%
          filter(case == TS_cases[i],year == years[j]) %>%
          mutate(year = as.character(year))
        temp_load_ts <- left_join(temp_load_ts,temp_flexible_demand) %>%
          pivot_longer(cols=c(`Load`, `Flexible Demand`),names_to = "Load_Type",values_to = "MW")
        temp_load_ts[is.na(temp_load_ts)] <- 0
      }
      if(!exists('load_timeseries')){
        load_timeseries <- temp_load_ts;
      } else {
        load_timeseries <- rbind(load_timeseries, temp_load_ts);
      }
    }
  }
  write_csv(load_timeseries,paste0(RunFdr,'/CompiledResults/',Subregions[k],'/Load/Load_fullyear_timeseries_',temp_total_title,'.csv'))
  rm(load_timeseries,temp_load_ts,temp_flexible_demand)
}


# 
for (k in 1:n_subregions){
  load_timeseries_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[k],'/Load/Load_fullyear_timeseries_',Subregions[k],'.csv')
  load_timeseries <- read_csv(load_timeseries_fn, col_types = cols())
  load_timeseries_original_summary <- load_timeseries %>%
    filter(Load_Type == 'Load') %>%
    group_by(case,year) %>%
    summarize(`Annual Peak Load` = max(MW),
              `Annual Average Load` = mean(MW))
  load_timeseries_flexmodified_summary <- load_timeseries %>%
    pivot_wider(names_from = 'Load_Type',values_from = 'MW') %>%
    group_by(case,year) %>%
    summarize(`Modified Annual Peak Load` = max(Load + `Flexible Demand`))
  load_timeseries_summary <- left_join(load_timeseries_original_summary,load_timeseries_flexmodified_summary);
  write_csv(load_timeseries_summary, paste0(RunFdr,'/CompiledResults/',Subregions[k],'/Load/Load_fullyear_summary_',temp_total_title,'.csv'))
}

