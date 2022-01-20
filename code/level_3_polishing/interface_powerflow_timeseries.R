# this file remap the interface powerflow to the full year

if (exists('interface_flow_full')){
  rm(interface_flow_full,interface_flow_reduced)
}

for (i in 1:length(TS_cases)){
  for (j in 1:length(years)){
    temp_powerflow_fn <- paste0(RunFdr,"/",years[j],"/",
                                TS_cases_id[i],"_",years[j],"_",TS_cases[i],
                                "/Results/flow.csv");
    temp_load_fn <- paste0(RunFdr,"/",years[j],"/",
                           TS_cases_id[i],"_",years[j],"_",TS_cases[i],
                           "/Inputs/Load_data.csv");
    temp_ts_mapping_fn <- paste0(RunFdr,"/",years[j],"/",
                                 TS_cases_id[i],"_",years[j],"_",TS_cases[i],
                                 "/Inputs/time_series_mapping.csv");
    if (file.exists(temp_powerflow_fn)){
      temp_interface_flow = read_csv(temp_powerflow_fn, col_types = cols()) %>%
        rename(Time_index = Line) %>%
        filter(Time_index != 'Sum') %>%
        select(-Total) %>%
        mutate(Time_index = as.numeric(str_remove(Time_index,'t'))) %>%
        pivot_longer(cols=c(-Time_index), names_to = 'Line') %>% 
        mutate(case = TS_cases[i], year = years[j]) %>%
        left_join(interface_line_mapping, by = c('Line' = 'Interface_Line')) %>%
        na.omit() %>%
        group_by(Time_index, Interface, case, year) %>%
        summarise(value = sum(Interface_Line_Direction*value)) %>%
        select(Time_index, Interface, case, year, value)
      
      if (file.exists(temp_ts_mapping_fn)){
        ts_mapping <- read_csv(temp_ts_mapping_fn)
        n_slot <- dim(ts_mapping)[1]
        Hours_per_period <- read_csv(temp_load_fn)$Hours_per_period %>% 
          na.omit()
        n_period <- read_csv(temp_load_fn)$Subperiods %>% na.omit()
        model_hours <- Hours_per_period*n_slot;
        HourID = c(1:model_hours)
        Slot <- rep(ts_mapping$slot,each = Hours_per_period)
        template <- as_tibble(cbind(HourID, Slot)) %>% 
          left_join(ts_mapping, by = c('Slot' = 'slot')) %>% 
          mutate(Time_index = rep(c(1:Hours_per_period),times = n_slot)) %>%
          mutate(Time_index = Time_index + Hours_per_period*(cluster-1))
        temp_interface_flow_full <- left_join(template, temp_interface_flow, 
                                              by = c('Time_index')) %>%
          select(HourID, Interface, case, year, value)
      } else {
        temp_interface_flow_full <- temp_interface_flow %>%
          rename(HourID = Time_index)
      }
    }
    if(!exists('interface_flow_full')){
      interface_flow_full <- temp_interface_flow_full;
      interface_flow_reduced <- temp_interface_flow;
    } else {
      interface_flow_full <- rbind(interface_flow_full, 
                                   temp_interface_flow_full);
      interface_flow_reduced <- rbind(interface_flow_reduced, 
                                      temp_interface_flow)
    }
  }
}
write_csv(interface_flow_full,
          paste0(RunFdr,'/CompiledResults/Interfacepowerflow_timeseries.csv'))
write_csv(interface_flow_reduced,
          paste0(RunFdr,
                 '/CompiledResults/Interfacepowerflow_timeseries_reduced.csv'))
rm(interface_flow_full,interface_flow_reduced,
   temp_interface_flow_full,temp_interface_flow,
   temp_powerflow_fn, temp_ts_mapping_fn, temp_load_fn)
interface_flow_full <- 
  read_csv(paste0(RunFdr,'/CompiledResults/Interfacepowerflow_timeseries.csv'))
ggplot(data=filter(interface_flow_full, Interface == 'PJM-NYISO'))+
  geom_line(aes(x = HourID, y=value,color=Interface)) +
  facet_grid(year~case)
  
  
