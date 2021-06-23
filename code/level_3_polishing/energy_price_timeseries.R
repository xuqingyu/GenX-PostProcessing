# This file remaps the reduced time-series of each zone into the original 8760 ts.

source('./code/Header.R')
if (exists('energyprice_ts')){
  rm(energyprice_ts,energyprice_reduced)
}

for (i in 1:length(TS_cases)){
  for (j in 1:length(years)){
    temp_price_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Results/prices_w.csv");
    temp_load_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/Load_data.csv");
    temp_ts_mapping_fn <- paste0(RunFdr,"/",years[j],"/",TS_cases_id[i],"_",years[j],"_",TS_cases[i],"/Inputs/time_series_mapping.csv");
    if (file.exists(temp_price_fn)){
      
      temp_price_ts = read_csv(temp_price_fn) %>% 
        rename(Time_index = Zone) %>% 
        mutate(Time_index = as.numeric(str_remove(Time_index,'t'))) %>% 
        pivot_longer(cols = !c(Time_index), names_to = 'Zone', values_to = 'Price') %>%
        mutate(Zone = as.character(round(as.numeric(Zone),0))) %>%
        left_join(zone_mapping, by= c('Zone' = 'zone')) %>%
        rename(Region = region)
      
      temp_price_ts$case <- TS_cases[i]
      temp_price_ts$year <- years[j]
      
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
        temp_price_ts_full <- left_join(template, temp_price_ts, by = c('Time_index')) %>%
          select(HourID, Zone, Price, Region, case,year)
      } else {
        temp_price_ts_full <- temp_price_ts %>%
          rename(HourID = Time_index)
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
}
write_csv(energyprice_ts,paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries.csv'))
write_csv(energyprice_reduced,paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries_reduced.csv'))
energyprice_ts_statistics <- energyprice_ts %>%
  group_by(Zone, Region, case, year) %>%
  summarize(`Price Mean` = mean(Price),
            `Price Std.` = sd(Price),
            `25th Quantile` = quantile(Price, 0.25),
            `Median` = quantile(Price, 0.50),
            `75th Quantile` = quantile(Price, .75))
write_csv(energyprice_ts_statistics,paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries_statistics.csv'))
rm(energyprice_ts,temp_price_ts,energyprice_reduced,temp_price_ts_full)





# energyprice_ts <- read_csv(paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries.csv'))
# temp_plot <- energyprice_ts  %>%
#   filter(case == 'deepdecarbonization_mid', year == 2050, grepl('PJM_NJ*',Region)) %>% 
#   filter(HourID %in% c(1:672))
# ggplot(data=temp_plot,aes(x=HourID, y=Price, color=Region)) +
#   geom_line() +
#   coord_cartesian(ylim = c(0,200)) +
#   geom_vline(xintercept = seq(from=1, to = 672, by = 24),color = 'grey90') +
#   theme_classic()+
#   ylab("Energy Price ($/MWh)")
# 
# temp_plot <- energyprice_ts  %>%
#   filter(year == 2050, grepl('PJM_NJ*',Region))
# ggplot(data=temp_plot,aes(x=case, y=Price)) +
#   geom_boxplot(outlier.shape = NA) +
#   theme_classic()+
#   coord_flip(ylim = c(0,200))+
#   ylab("Energy Price ($/MWh)")
# 
# 
# energyprice_reduced <- read_csv(paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries_reduced.csv'))
# energyprice_reduced_statistics <- energyprice_reduced %>%
#   mutate(cluster = ceiling(Time_index/168)) %>%
#   group_by(Region, case,year,cluster) %>%
#   summarize(`Avg.Price` = mean(Price))
# temp_plot <- energyprice_reduced_statistics  %>%
#   filter(grepl('PJM_NJ*',Region),grepl('statedpolicy_',case),grepl('_mid',case)) 
# ggplot(data=temp_plot,aes(x=cluster, y=Avg.Price, color=Region)) +
#   geom_point() +
#   coord_cartesian(ylim = c(0,100)) +
#   geom_vline(xintercept = c(1:18),color="grey90")+
#   theme_classic()+
#   facet_grid(year~case)+
#   ylab("Avg.Price ($/MWh)")
