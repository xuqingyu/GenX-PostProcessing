# This file remaps the reduced time-series of each zone into the original 8760 ts.

source('./code/Header.R')
if (exists('capacityprice_ts')){
  rm(capacityprice_ts)
}

for (i in 1:length(TS_cases)){
  for (j in 1:length(years)){
    temp_capacity_price_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Results/ReserveMargin_w.csv");
    temp_load_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/Load_data.csv");
    temp_ts_mapping_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/time_series_mapping.csv");
    if (file.exists(temp_capacity_price_fn)){
      
      temp_capacity_price = read_csv(temp_capacity_price_fn) %>% 
        rename(Time_index = Constraint) %>% 
        mutate(Time_index = as.numeric(str_remove(Time_index,'t'))) %>% 
        pivot_longer(cols = !c(Time_index), names_to = 'Constraint', values_to = 'Price') %>%
        mutate(Price = round(Price, 3))
      temp_capacity_price$Price[temp_capacity_price$Price == 0] <- NA
      temp_capacity_price$case <- TS_cases[i]
      temp_capacity_price$year <- years[j]
      
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
        temp_capacity_price <- left_join(template, temp_capacity_price, by = c('Time_index')) %>%
          select(HourID, Constraint, Price, case,year)
      } else {
        temp_capacity_price <- temp_capacity_price %>%
          rename(HourID = Time_index)
      }
    }
    if(!exists('capacityprice_ts')){
      capacityprice_ts <- temp_capacity_price;
    } else {
      capacityprice_ts <- rbind(capacityprice_ts, temp_capacity_price);
    }
  }
}
write_csv(capacityprice_ts,paste0(RunFdr,'/CompiledResults/CapacityPrice_timeseries.csv'))
capacityprice_ts_statistics <- capacityprice_ts %>%
  na.omit() %>%
  group_by(Constraint, case, year) %>%
  summarize(`Annual Price` = sum(Price)/365,
            `Number of Critical Hours` = sum(Price>0))
write_csv(capacityprice_ts_statistics,paste0(RunFdr,'/CompiledResults/CapacityPrice_timeseries_statistics.csv'))
rm(capacityprice_ts_statistics,temp_capacity_price,capacityprice_ts)





# capacityprice_ts <- read_csv(paste0(RunFdr,'/CompiledResults/CapacityPrice_timeseries.csv')) %>%
#   mutate(Binding = ifelse(is.na(Price),NA,1))
# temp_plot <- capacityprice_ts  %>%
#   filter(year == 2030)
# ggplot(data=temp_plot,aes(x=HourID, y=Binding, color=Constraint)) +
#   geom_point() +
#   # coord_flip() +
#   scale_x_reverse()+
#   theme_classic()+
#   facet_grid(case~.)+
#   ylab("Binding Hour")

