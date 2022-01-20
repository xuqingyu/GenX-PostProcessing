# Google Emission Calculation ----
# Plant Level emission calculation [add this function in]

# CFE emissions ----
# we are using the captured emission for current calculation because GenX 
# does not produce plant level emission rate with the current version
print('begin compiling emissions')
print(Sys.time())
cfe_emission_table = tibble(
  case = character(),
  year = character(),
  cfeemission = numeric(),
  shortfallemission_local = numeric(), 
  # shortfall emission rate is using rest of PJM/CA emission rate
  shortfallemission_system = numeric(),
  # shortfall emission rate is using rest of PJM or CA using 
  # [rest of CA emission + hourly imported emission]/[rest of CA Gen + hourly import]
  emission_local = numeric(),
  # emission rate is using the PJM/CA emission rate
  emission_system = numeric(),
  # emission rate is using the PJM or CA using 
  # [CA emission + hourly imported emission]/[CA Gen + hourly import]
  `Participated Load` = numeric()
)

cfe_hourly_emission = tibble(
  case = character(),
  year = character(),
  Time_index = numeric(),
  `Rest of Local Emission Rate` = numeric(),
  `Rest of Local and Import Emission Rate` = numeric(),
  `Local Emission Rate` = numeric(),
  `Local and Import Emission Rate` = numeric()
)

cfe_system_emission_table = tibble(
  case = character(),
  year = character(),
  emission_local = numeric(),
  # emission rate is using the PJM/CA emission rate
  emission_system = numeric(),
  # emission rate is using the PJM or CA using 
  # [CA emission + hourly imported emission]/[CA Gen + hourly import]
  `Local Load` = numeric()
)
k=1
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_emission_captured_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                               years[j],"_",cases[i],"/Results/emissions_captured.csv");
    temp_zonal_emission_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                  years[j],"_",cases[i],"/Results/emissions.csv");
    temp_timeweight_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Results/time_weights.csv");
    temp_power_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                            years[j],"_",cases[i],"/Results/power.csv");
    temp_sf_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                         years[j],"_",cases[i],"/Results/RPSH_SF.csv");
    temp_cfeload_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                              years[j],"_",cases[i],"/Inputs/Load_data_RPSH.csv");
    temp_load_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Inputs/Load_data.csv");
    temp_power_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                            years[j],"_",cases[i],"/Results/power.csv");
    temp_charge_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Results/charge.csv");
    temp_generator <- read_csv(temp_generator_fn, col_types = cols())
    temp_timeweight = read_csv(temp_timeweight_fn,col_types = cols())
    temp_demand <- read_csv(temp_load_fn, col_types = cols()) %>%
      select(-c(1:9))
    if (Studyregion == 'PJM'){
      local_load = rowSums(temp_demand[,c(5:13)])
    }
    if (Studyregion == 'WECC'){
      local_load = rowSums(temp_demand[,c(1:2)])
    }
    # cfe_gen_rows = which((temp_generator$RPSH_1 >0) & (temp_generator$DR == 0))
    # cfe_ccs_rows = intersect(cfe_gen_rows, which(temp_generator$CO2_Capture_Rate > 0))
    # gen_rows = which((temp_generator$DR == 0))
    # if (Studyregion == 'PJM'){
    #   local_gen_rows = intersect(gen_rows, grep('PJM_',temp_generator$region))
    # }
    # if (Studyregion == 'WECC'){
    #   local_gen_rows = intersect(gen_rows, grep('CA_',temp_generator$region))
    # }
    # rest_of_local_gen_rows = setdiff(local_gen_rows, cfe_gen_rows)
    # rest_of_system_gen_rows = setdiff(gen_rows, local_gen_rows)
    power_ts = read_csv(temp_power_fn, col_names = F, skip = 3, col_types = cols())
    power_ts = power_ts[,-c(1,ncol(power_ts))]
    charge_ts = read_csv(temp_charge_fn, col_names = F, skip = 3, col_types = cols())
    charge_ts = charge_ts[,-c(1,ncol(charge_ts))]
    # local_power = rowSums(power_ts[,local_gen_rows] - charge_ts[,local_gen_rows])
    # rest_of_local_power = rowSums(power_ts[,rest_of_local_gen_rows] - charge_ts[,rest_of_local_gen_rows])
    # rest_of_system_power = rowSums(power_ts[,rest_of_system_gen_rows] - charge_ts[,rest_of_system_gen_rows])
    # 
    # net_import = local_load - local_power
    # net_import[which(net_import <0)] <- 0
    # net_export = local_power - local_load
    # net_export[which(net_export <0)] <- 0
    policy_col = which(colnames(temp_generator)==paste0('RPSH_',k))
    storage_col = which(colnames(temp_generator)==paste0('STOR'))
    dr_col = which(colnames(temp_generator)==paste0('DR'))
    storage_rows = which(temp_generator[, storage_col] >= 1)
    dr_rows = which(temp_generator[,dr_col] >=1)
    cfe_rows = which(temp_generator[,policy_col] >0)
    cfe_ccs_rows = intersect(cfe_rows, which(temp_generator$CO2_Capture_Rate > 0))
    if (Studyregion == 'PJM'){
      local_rows = grep('PJM_', temp_generator$region)
    }
    if (Studyregion == 'WECC'){
      local_rows = grep('CA_', temp_generator$region)
    }        
    local_injection_rows = setdiff(local_rows, dr_rows) # Local gen + storage
    rest_of_local_injection_rows = setdiff(local_injection_rows, cfe_rows)
    local_withdraw_rows = intersect(local_rows, storage_rows)# Local storage
    local_dr_rows = intersect(local_rows, dr_rows)
    rest_of_system_injection_rows = setdiff(which(temp_generator[,dr_col] ==0), local_injection_rows)
    local_power = rowSums(power_ts[,local_injection_rows]) # total injection into CA/PJM
    rest_of_local_power = rowSums(power_ts[,rest_of_local_injection_rows])
    rest_of_system_power = rowSums(power_ts[,rest_of_system_injection_rows])
    
    net_interchange = (local_load + 
                         rowSums(power_ts[,local_dr_rows] - charge_ts[,local_dr_rows]) + # DR modification
                         rowSums(charge_ts[,local_withdraw_rows]) - 
                         local_power)
    net_import = net_interchange
    net_import[which(net_import <0)] <- 0
    net_export = net_interchange
    net_export[which(net_export >0)] <- 0    
    emission_captured = read_csv(temp_emission_captured_fn, 
                                   col_types = cols(),
                                   col_names = F,
                                   skip = 6)[,-1]
    cfe_emissions_hourly = rowSums((emission_captured[,cfe_ccs_rows]/
                       t(temp_generator$CO2_Capture_Rate[cfe_ccs_rows])*
                       t(1-temp_generator$CO2_Capture_Rate[cfe_ccs_rows])))
    n_hour = length(local_power)
    if (length(cfe_emissions_hourly) == 0){
      cfe_emissions_hourly = rep(0, n_hour)
    }
    cfe_emissions_row = 0
    if (file.exists(temp_sf_fn)){
      cfe_emissions_row = cfe_emissions_hourly %*% temp_timeweight$Weight;      
    }
    temp_zonal_emission = read_csv(temp_zonal_emission_fn, 
                                   col_types = cols(),
                                   col_names = F,
                                   skip = 2);
    temp_zonal_emission <- temp_zonal_emission[,-c(1,ncol(temp_zonal_emission))]
    if (Studyregion == 'PJM'){
      local_emission_hourly = rowSums(temp_zonal_emission[,c(5:13)])
    }
    if (Studyregion == 'WECC'){
      local_emission_hourly = rowSums(temp_zonal_emission[,c(1:2)])
    }
    rest_of_local_emission_hourly = local_emission_hourly - cfe_emissions_hourly
    system_emission_hourly = rowSums(temp_zonal_emission) #hourly emission of WECC/PJM
    rest_of_system_emission_hourly = system_emission_hourly - local_emission_hourly
    rest_of_system_emission_rate_hourly = rest_of_system_emission_hourly/rest_of_system_power
    net_imported_emission = net_import*rest_of_system_emission_rate_hourly
    
    # Emission rate calculation
    rest_of_local_emission_rate_hourly = rest_of_local_emission_hourly/rest_of_local_power
    rest_of_local_plus_import_emission_rate_hourly = (net_imported_emission + rest_of_local_emission_hourly)/(rest_of_local_power + net_import)
    local_emission_rate_hourly = local_emission_hourly/local_power
    local_plus_import_emission_rate_hourly = (local_emission_hourly + net_imported_emission)/(local_power + net_import)
    shortfallemission_local_row = 0
    shortfallemission_system_row = 0
    if(file.exists(temp_sf_fn)) {
      shortfall = t(read_csv(temp_sf_fn,col_types = cols()))
      shortfallemission_local_row = t(shortfall * rest_of_local_emission_rate_hourly) %*% temp_timeweight$Weight
      shortfallemission_system_row = t(shortfall * rest_of_local_plus_import_emission_rate_hourly) %*% temp_timeweight$Weight
    }
    
    cfe_load = rowSums(read_csv(temp_cfeload_fn,col_types = cols())[,-1])
    emission_local_row = t(cfe_load * local_emission_rate_hourly) %*% temp_timeweight$Weight
    emission_system_row = t(cfe_load * local_plus_import_emission_rate_hourly) %*% temp_timeweight$Weight
    participated_load_row = t(cfe_load) %*% temp_timeweight$Weight
    temp_row = as_tibble_row(c(
      case = cases[i],
      year = years[j],
      cfeemission = cfe_emissions_row,
      shortfallemission_local = shortfallemission_local_row, 
      shortfallemission_system = shortfallemission_system_row,
      emission_local = emission_local_row,
      emission_system = emission_system_row,
      `Participated Load` = participated_load_row)
    )
    cfe_emission_table = rbind(cfe_emission_table, temp_row)

    emission_local_all_row = t(local_load * local_emission_rate_hourly) %*% temp_timeweight$Weight
    # this is the emission that the load is responsible
    emission_system_all_row = t(local_load * local_plus_import_emission_rate_hourly) %*% temp_timeweight$Weight
    local_load_row = t(local_load) %*% temp_timeweight$Weight
    temp_row = as_tibble_row(c(
      case = cases[i],
      year = years[j],
      emission_local = emission_local_all_row,
      emission_system = emission_system_all_row,
      `Local Load` = local_load_row)
    )
    cfe_system_emission_table = rbind(cfe_system_emission_table, temp_row)
    
    n_hour = length(local_power)
    temp_matrix = as_tibble(cbind(
      case = rep(cases[i],n_hour),
      year = rep(years[j],n_hour),
      Time_index = c(1:n_hour),
      `Rest of Local Emission Rate` = rest_of_local_emission_rate_hourly,
      `Rest of Local and Import Emission Rate` = rest_of_local_plus_import_emission_rate_hourly,
      `Local Emission Rate` = local_emission_rate_hourly,
      `Local and Import Emission Rate` =local_plus_import_emission_rate_hourly
    ))
    cfe_hourly_emission = rbind(cfe_hourly_emission, temp_matrix)
  }
}
cfe_emission_table_withname <- cfe_emission_table %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(cfe_emission_table_withname, paste0(temp_RunFdr,'/CompiledResults/ci_emissions.csv'))

cfe_system_emission_table_withname <- cfe_system_emission_table %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(cfe_system_emission_table_withname, paste0(temp_RunFdr,'/CompiledResults/system_emissions.csv'))

cfe_hourly_emission_withname <- cfe_hourly_emission %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(cfe_hourly_emission_withname, paste0(temp_RunFdr,'/CompiledResults/system_emissions_hourly.csv'))

# mapping it back to 8760 ----

cfe_hourly_emission_withname <- read_csv(paste0(temp_RunFdr,'/CompiledResults/system_emissions_hourly.csv')) %>%
  select(case,year,Time_index,Scenario,TechSensitivity,
         `Rest of Local Emission Rate`,`Rest of Local and Import Emission Rate`,
         `Local Emission Rate`, `Local and Import Emission Rate`)

if(exists('cfe_hourly_emission_full')) { rm(cfe_hourly_emission_full)}


for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_load_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Load_data.csv");
    temp_ts_mapping_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/time_series_mapping.csv");
    temp_ts_emission = cfe_hourly_emission_withname %>% filter(case == cases[i], year == years[j])
    if(nrow(temp_ts_emission) >0){
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
        temp_ts_emission_full <- left_join(template, temp_ts_emission, by = c('Time_index')) %>%
          select(-c(Slot, cluster, Month, Time_index))
      }
      if(!exists('cfe_hourly_emission_full')){
        cfe_hourly_emission_full <- temp_ts_emission_full;
      } else {
        cfe_hourly_emission_full <- rbind(cfe_hourly_emission_full, temp_ts_emission_full)
      }
      print(i)
    }
  }
}
write_csv(cfe_hourly_emission_full, paste0(temp_RunFdr,"/CompiledResults/CFE_emissionrate_timeseries_full.csv"))

print('end compiling emissions')
print(Sys.time())
