# This file remaps the reduced time-series of each zone into the original 8760 ts.

if (exists('capacityprice_ts')){
  rm(capacityprice_ts)
}
counter = 1
if (length(TS_cases) > 0) {
  k=1
  for (i in 1:length(TS_cases)){
    for (j in length(years)){ # only the last year is calculated on purpose
      temp_capacity_price_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Results/ReserveMargin.csv");
      temp_load_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/Load_data.csv");
      temp_ts_mapping_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/Period_map.csv");
      if (file.exists(temp_capacity_price_fn)){
        
        temp_capacity_price = read_csv(temp_capacity_price_fn, col_types = cols()) %>% 
          rename(Time_Index = Constraint) %>% 
          mutate(Time_Index = as.numeric(str_remove(Time_Index,'t'))) %>% 
          pivot_longer(cols = !c(Time_Index), names_to = 'Constraint', values_to = 'Price') %>%
          mutate(Price = round(Price, 3))
        temp_capacity_price$Price[temp_capacity_price$Price == 0] <- NA
        temp_capacity_price$case <- TS_cases[i]
        temp_capacity_price$year <- years[j]
        
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
            mutate(Time_Index = Time_Index + Hours_per_period*(Rep_Period_Index-1))
          temp_capacity_price <- left_join(template, temp_capacity_price, by = c('Time_Index')) %>%
            select(HourID, Constraint, Price, case,year)
        } else {
          temp_capacity_price <- temp_capacity_price %>%
            rename(HourID = Time_index)
        }
        
        if(!exists('capacityprice_ts')){
          capacityprice_ts <- temp_capacity_price;
        } else {
          capacityprice_ts <- rbind(capacityprice_ts, temp_capacity_price);
        }
        counter = counter + 1
      }
      print(counter)
      if ((counter %% 10 == 0)| (i == length(TS_cases))) {
        write_csv(capacityprice_ts,paste0(RunFdr,'/CompiledResults/CapacityPrice_timeseries_',k,'.csv'))
        rm(capacityprice_ts,capacityprice_ts)
        k = k +1
        print(k)
      }
    }
  }
  # write_csv(capacityprice_ts,paste0(RunFdr,'/CompiledResults/CapacityPrice_timeseries.csv'))
  
  maxk = k - 1
  k = 1
  for (k in (1:maxk)){
    temp_capacity_price <- read_csv(paste0(RunFdr,'/CompiledResults/CapacityPrice_timeseries_',k,'.csv'),
                                    col_types = cols(Price = col_double()))
    temp_capacity_price$Price[is.na(temp_capacity_price$Price)] <- 0
    if(!exists('capacityprice_ts')){
      capacityprice_ts <- temp_capacity_price;
    } else {
      capacityprice_ts <- rbind(capacityprice_ts, temp_capacity_price);
    }
    print(k)
  }
  capacityprice_ts_statistics <- capacityprice_ts %>%
    na.omit() %>%
    group_by(Constraint, case, year) %>%
    summarize(`Annual Price` = sum(Price)/365,
              `Number of Critical Hours` = sum(Price>0))
  write_csv(capacityprice_ts_statistics,paste0(RunFdr,'/CompiledResults/CapacityPrice_timeseries_statistics.csv'))
  rm(capacityprice_ts_statistics,temp_capacity_price,capacityprice_ts)
} 

