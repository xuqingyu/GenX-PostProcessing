# Combining Power result for Google CFE project----

cfe_table = tibble(
  case = character(),
  year = character(),
  `Policy` = character(),
  `Shortfall` = numeric(),
  `Shortfall price` = numeric(),
  `Excess` = numeric(),
  `Excess price` = numeric(),
  Load = numeric(),
  `Storage loss` = numeric(),
  `Pre-Grid CFE Score` = numeric(),
  `Post-Grid CFE Score Local` = numeric(),
  `Post-Grid CFE Score Local_n_Import` = numeric(),
  `Local Cleanness` = numeric(),
  `Local_n_Import Cleanness` = numeric()
)


cfe_ts_allcase_cfe = tibble(
  case = character(),
  year = character(),
  Time_index = numeric(),
  name = character(),
  value = numeric()
)

cfe_ts_allcase_gen = tibble(
  case = character(),
  year = character(),
  Time_index = numeric(),
  name = character(),
  value = numeric()
)

cfe_ts_allcase_load_dr_stor = tibble(
  case = character(),
  year = character(),
  Time_index = numeric(),
  name = character(),
  value = numeric()
)

cfe_hourly_cleanness = tibble(
  case = character(),
  year = character(),
  `Policy` = character(),
  Time_index = numeric(),
  `Rest of Local Cleanness` = numeric(),
  `Rest of Local and Import Cleanness` = numeric(),
  `Local Cleanness` = numeric(),
  `Local and Import Cleanness` = numeric()
)

cfe_capacity_allcase = tibble(
  case = character(),
  year = character(),
  `Policy` = character(),
  name = character(),
  value = numeric()
)

cfe_emission_table = tibble(
  case = character(),
  year = character(),
  cfeemission = numeric(),
  shortfallemission_local = numeric(), 
  # shortfall emission rate is using rest of PJM/CA emission rate
  shortfallemission_local_n_import = numeric(),
  # shortfall emission rate is using rest of PJM or CA using 
  # [rest of CA emission + hourly imported emission]/[rest of CA Gen + hourly import]
  emission_local = numeric(),
  # emission rate is using the PJM/CA emission rate
  emission_local_n_import = numeric(),
  emission_measure = numeric(),
  # emission rate is using the PJM or CA using 
  # [CA emission + hourly imported emission]/[CA Gen + hourly import]
  `Participated Load` = numeric(),
  `Storage Loss` = numeric()
)

cfe_hourly_emission = tibble(
  case = character(),
  year = character(),
  Time_index = numeric(),
  `Rest of Local Emission Rate` = numeric(),
  `Rest of Local and Import Emission Rate` = numeric(),
  `Local Emission Rate` = numeric(),
  `Local and Import Emission Rate` = numeric(),
  `Total Withdraw` = numeric()
)

cfe_system_emission_table = tibble(
  case = character(),
  year = character(),
  emission_local = numeric(),
  # emission rate is using the PJM/CA emission rate
  emission_local_n_import = numeric(),
  emission_rest_of_local = numeric(),
  emission_rest_of_local_w_import = numeric(),
  participating_load_emission = numeric(),
  # emission rate is using the PJM or CA using 
  # [CA emission + hourly imported emission]/[CA Gen + hourly import]
  `Local Load` = numeric()
)
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_cfe_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                          years[j],"_",cases[i],"/Results/RPSH_CFE.csv");
    temp_ex_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                         years[j],"_",cases[i],"/Results/RPSH_EX.csv");
    temp_sf_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                         years[j],"_",cases[i],"/Results/RPSH_SF.csv");
    temp_sf_dirtiness_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                   years[j],"_",cases[i],"/Inputs/RPSH_SFDT.csv");
    temp_timeweight_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Results/time_weights.csv");
    temp_cfeload_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                              years[j],"_",cases[i],"/Inputs/Load_data_RPSH.csv");
    temp_cfepolicy_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/RPSH.csv");
    temp_generator_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_charge_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Results/charge.csv");
    temp_power_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                            years[j],"_",cases[i],"/Results/power.csv");
    temp_capacity_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                               years[j],"_",cases[i],"/Results/capacity.csv");
    temp_load_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                           years[j],"_",cases[i],"/Inputs/Load_data.csv");
    #Note the hourly cfe prices are with timeweights.
    temp_hourcfeprice_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                   years[j],"_",cases[i],"/Results/RPSH_Hourlymatchingdual.csv");
    temp_sf_shadowprice_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                     years[j],"_",cases[i],"/Results/RPSH_SFlimitdual.csv");
    temp_ex_shadowprice_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                     years[j],"_",cases[i],"/Results/RPSH_EXlimitdual.csv");
    temp_emission_captured_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                        years[j],"_",cases[i],"/Results/emissions_captured.csv");
    temp_zonal_emission_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                     years[j],"_",cases[i],"/Results/emissions.csv");
    if (file.exists(temp_timeweight_fn)) {
      # Calculate the hourly local demand, for the calculation of the import
      temp_demand <- read_csv(temp_load_fn, col_types = cols()) %>%
        select(-c(1:9))
      if (Studyregion == 'PJM'){
        local_load = rowSums(temp_demand[,c(5:13)])
        n_hour = length(local_load)
      }
      if (Studyregion == 'WECC'){
        local_load = rowSums(temp_demand[,c(1:2)])
        n_hour = length(local_load)
      }
      rm(temp_demand)
      temp_timeweight <- read_csv(temp_timeweight_fn, col_types = cols())
      temp_generator <- read_csv(temp_generator_fn, col_types = cols()) %>%
        left_join(resource_mapping_includingflexibleload, by = c('Resource' = 'All_Resource'))
      
      # skip the first three columns
      power_ts = read_csv(temp_power_fn, col_names = F, skip = 3, col_types = cols())
      # remove the last column of total and the first column of time
      power_ts = power_ts[,-c(1,ncol(power_ts))]
      power_ts_transposed <- t(power_ts)
      colnames(power_ts_transposed) = c(1:n_hour)
      power_ts_transposed <- as_tibble(power_ts_transposed)
      
      charge_ts = read_csv(temp_charge_fn, col_names = F, skip = 3, col_types = cols())
      # remove the last column of total and the first column of time
      charge_ts = charge_ts[,-c(1,ncol(charge_ts))]
      charge_ts_transposed <- t(charge_ts)
      colnames(charge_ts_transposed) = c(1:n_hour)
      charge_ts_transposed <- as_tibble(charge_ts_transposed)
      temp_cfeload_all <- read_csv(temp_cfeload_fn, col_types = cols()) # in all case, this file should be there.
      if (Studyregion == 'PJM'){
        cfe_zones = c(5:13)
      }
      if (Studyregion == 'WECC'){
        cfe_zones = c(1:2)
      }
      for (k in 1:n_cfe) {
        cfe_load = rowSums(temp_cfeload_all[,cfe_zones+1])
        cfe_load_hourly = as_tibble(cfe_load) %>%
          mutate(case = cases[i],
                 year = years[j],
                 policy = paste0('RPSH_',k),
                 Time_index = c(1:n_hour),
                 name = 'Load') %>%
          select(case,year,policy, Time_index,name, value)
        cfe_annualload = cfe_load_hourly %>%
          mutate(Weight = temp_timeweight$Weight) %>%
          group_by(case, year, policy, name) %>%
          summarize(value = sum(Weight*value)) %>%
          select(case,year,policy,name, value)
        cfe_ts_allcase_load_dr_stor = rbind(cfe_ts_allcase_load_dr_stor, cfe_load_hourly)
        
        # calculate hourly dr, and storage
        
        policy_col = which(colnames(temp_generator)==paste0('RPSH_',k)) # locate the policy column
        storage_col = which(colnames(temp_generator)==paste0('STOR')) # locate the storage column
        dr_col = which(colnames(temp_generator)==paste0('DR')) # locate the DR column
        # figure out which rows are storage
        cfe_storage_rows = which((temp_generator[,policy_col] > 0) & (temp_generator[, storage_col] >= 1))
        cfe_storage_ts_discharge = power_ts_transposed[cfe_storage_rows,] %>%
          colSums() %>% 
          as_tibble_col() %>%
          mutate(case = cases[i],
                 year = years[j],
                 policy = paste0('RPSH_',k),
                 Time_index = c(1:n_hour),
                 name = 'Storage Discharge') %>%
          select(case,year,policy, Time_index,name, value)
        cfe_ts_allcase_load_dr_stor = rbind(cfe_ts_allcase_load_dr_stor, cfe_storage_ts_discharge)
        cfe_storage_ts_charge = charge_ts_transposed[cfe_storage_rows,] %>%
          colSums() %>% 
          as_tibble_col() %>%
          mutate(case = cases[i],
                 year = years[j],
                 policy = paste0('RPSH_',k),
                 Time_index = c(1:n_hour),
                 name = 'Storage Charge') %>%
          select(case,year,policy, Time_index,name, value)
        cfe_ts_allcase_load_dr_stor = rbind(cfe_ts_allcase_load_dr_stor, cfe_storage_ts_charge)
        
        # calcualte the annual loss from storage.
        temp_loss = numeric()
        temp_loss = sum(cfe_storage_ts_charge$value*temp_timeweight$Weight) - 
          sum(cfe_storage_ts_discharge$value*temp_timeweight$Weight)
        temp_loss = as_tibble_row(c(value = temp_loss)) %>%
          mutate(case = cases[i],
                 year = years[j],
                 policy = paste0('RPSH_',k),
                 name = 'Storage Loss') %>%
          select(case,year,policy, name, value)
        
        # figure out which rows are demand response
        cfe_dr_rows = which((temp_generator[,policy_col] > 0) & (temp_generator[, dr_col] >= 1))
        cfe_dr_loadincrease = (power_ts_transposed[cfe_dr_rows,]*
                                 rep(temp_generator[cfe_dr_rows,policy_col],n_hour)) %>%
          colSums() %>% 
          as_tibble_col() %>%
          mutate(case = cases[i],
                 year = years[j],
                 policy = paste0('RPSH_',k),
                 Time_index = c(1:n_hour),
                 name = 'DR Load Increase') %>%
          select(case,year,policy, Time_index,name, value)
        cfe_ts_allcase_load_dr_stor = rbind(cfe_ts_allcase_load_dr_stor, cfe_dr_loadincrease)
        
        cfe_dr_loaddecrease = (charge_ts_transposed[cfe_dr_rows,]*
                                 rep(temp_generator[cfe_dr_rows,policy_col],n_hour)) %>%
          colSums() %>% 
          as_tibble_col() %>%
          mutate(case = cases[i],
                 year = years[j],
                 policy = paste0('RPSH_',k),
                 Time_index = c(1:n_hour),
                 name = 'DR Load Decrease') %>%
          select(case,year,policy, Time_index,name, value)
        cfe_ts_allcase_load_dr_stor = rbind(cfe_ts_allcase_load_dr_stor, cfe_dr_loaddecrease)
        
        # Calculate Hourly Modified Load
        cfe_modified_load = rbind(cfe_load_hourly, 
                                  cfe_storage_ts_discharge,
                                  cfe_storage_ts_charge,
                                  cfe_dr_loadincrease,
                                  cfe_dr_loaddecrease) %>%
          pivot_wider(names_from = 'name',values_from = 'value') %>%
          mutate(value = (`Load` - `Storage Discharge` + `Storage Charge` + `DR Load Increase` - `DR Load Decrease`),
                 name = 'Modified Load') %>%
          select(case, year, policy, Time_index, name, value)
        cfe_ts_allcase_load_dr_stor = rbind(cfe_ts_allcase_load_dr_stor, cfe_modified_load)
        
        cfe_load_afterdr = rbind(cfe_load_hourly, 
                                  cfe_dr_loadincrease,
                                  cfe_dr_loaddecrease) %>%
          pivot_wider(names_from = 'name',values_from = 'value') %>%
          mutate(value = (`Load` + `DR Load Increase` - `DR Load Decrease`),
                 name = 'Load after DR') %>%
          select(case, year, policy, Time_index, name, value)
        
        # Calculate Hourly Generation
        cfe_gen_rows_nostor = which((temp_generator[,policy_col]>0) &
                                      (temp_generator[,dr_col] == 0) & 
                                      (temp_generator[,storage_col] == 0))
        # these are the rows for CFE eligible power generating generators.
        cfe_Resource = temp_generator$All_Fuel
        cfe_gen <- cbind(cfe_Resource, power_ts_transposed)[cfe_gen_rows_nostor,] %>%
          pivot_longer(cols = !c(cfe_Resource), names_to = 'Time_index') %>%
          group_by(cfe_Resource, Time_index) %>%
          summarize(value = sum(value)) %>%
          mutate(case = cases[i],
                 year = years[j],
                 policy = paste0('RPSH_',k),
                 Time_index = as.numeric(Time_index)) %>%
          rename(name = cfe_Resource) %>%
          select(case,year,policy, Time_index,name, value)
        cfe_ts_allcase_gen = rbind(cfe_ts_allcase_gen, cfe_gen)
        
        if (exists('temp_cfepolicy')) {rm(temp_cfepolicy)}
        if (exists('temp_cfe')){
          rm(temp_sf_shadowprice, temp_ex_shadowprice,temp_hourcfeprice)
          rm(temp_cfe, temp_cfe_hourly, temp_cfe_total)
          rm(temp_ex, temp_ex_hourly, temp_ex_total)
          rm(temp_sf, temp_sf_hourly, temp_sf_total)
        }

        if (!grepl('nocip',cases[i])){
          if (file.exists(temp_cfe_fn)) {
            
            temp_cfepolicy <- read_csv(temp_cfepolicy_fn, col_types = cols())
            temp_sf_shadowprice <- read_csv(temp_sf_shadowprice_fn, col_types = cols())[k]
            temp_ex_shadowprice <- read_csv(temp_ex_shadowprice_fn, col_types = cols())[k]
            temp_hourcfeprice <- read_csv(temp_hourcfeprice_fn, col_types = cols())[,k]
            # CFE without subtracting the excess
            temp_cfe <- t(read_csv(temp_cfe_fn, col_types = cols()))[,k]; # the kth column is the RPSH_k
            temp_cfe_hourly <- as_tibble(temp_cfe) %>%
              mutate(case = cases[i],
                     year = years[j],
                     policy = paste0('RPSH_',k),
                     Time_index = c(1:n_hour),
                     name = 'CFE') %>%
              select(case,year,policy, Time_index,name, value)
            temp_cfe_total <- temp_cfe_hourly %>%
              mutate(Weight = temp_timeweight$Weight) %>%
              group_by(case, year, policy,name) %>%
              summarize(value = sum(Weight*value)) %>%
              select(case,year,policy, name, value)
            # Shortfall
            # Step 1: read in the shortfall and format
            temp_sf_temp <- t(read_csv(temp_sf_fn, col_types = cols()))[,k];
            temp_ex_temp <- t(read_csv(temp_ex_fn, col_types = cols()))[,k];
            
            temp_sf <- (temp_sf_temp - temp_ex_temp)
            temp_sf[temp_sf<0] <- 0
            temp_ex <- (temp_ex_temp - temp_sf_temp)
            temp_ex[temp_ex<0] <- 0
            
            temp_sf_hourly <- as_tibble(temp_sf) %>%
              mutate(case = cases[i],
                     year = years[j],
                     policy = paste0('RPSH_',k),
                     Time_index = c(1:n_hour),
                     name = 'Shortfall') %>%
              select(case,year,policy, Time_index,name, value)
            # Step 2: read in the dirtiness
            if (!file.exists(temp_sf_dirtiness_fn)){
              temp_sf_dirtiness = as_tibble_col(c(rep(1,n_hour)))
            } else {
              temp_sf_dirtiness <- read_csv(temp_sf_dirtiness_fn, col_types = cols())[,k+1];
            }
            temp_sf_total <- temp_sf_hourly %>%
              mutate(Weight = temp_timeweight$Weight,
                     Dirtiness = temp_sf_dirtiness) %>%
              group_by(case, year, policy,name) %>%
              summarize(value = sum(Weight*value*Dirtiness)) %>%
              select(case,year,policy, name, value)
            
            # Excess
 
            # fixing the ultra high ex problem
            # zero_dirtyrows = which(temp_sf_dirtiness == 0)
            # temp_ex[zero_dirtyrows] = temp_ex[zero_dirtyrows] - temp_sf[zero_dirtyrows]
            # temp_ex[which(temp_ex<0)] <- 0
            
            temp_ex_hourly <- as_tibble(temp_ex) %>%
              mutate(case = cases[i],
                     year = years[j],
                     policy = paste0('RPSH_',k),
                     Time_index = c(1:n_hour),
                     name = 'Excess') %>%
              select(case,year,policy, Time_index,name, value)
            temp_ex_total <- temp_ex_hourly %>%
              mutate(Weight = temp_timeweight$Weight) %>%
              group_by(case, year, policy,name) %>%
              summarize(value = sum(Weight*value)) %>%
              select(case,year,policy, name, value)

            
          } else {
            temp_sf_shadowprice <- 0
            temp_ex_shadowprice <- 0
            temp_hourcfeprice <- rep(0,n_hour)
            temp_cfe_hourly <- cfe_gen %>%
              group_by(case, year, policy, Time_index) %>%
              summarize(value = sum(value)) %>%
              mutate(name = 'CFE') %>%
              select(case,year,policy, Time_index, name, value)
            temp_cfe_total <- temp_cfe_hourly %>%
              mutate(Weight = temp_timeweight$Weight) %>%
              group_by(case, year, policy,name) %>%
              summarize(value = sum(Weight*value)) %>%
              select(case,year,policy, name, value)
            
            temp_sf_ex_calculation <- left_join(temp_cfe_hourly, cfe_load_afterdr, # note that this is not modified load because there is no incentive for annual matching case to build storage
                                                by = c('case','year','policy','Time_index'))
            temp_ex_hourly <- temp_sf_ex_calculation %>%
              mutate(value = value.x-value.y)
            temp_ex_hourly$value[which(temp_ex_hourly$value <0)] <- 0
            temp_ex_hourly <- temp_ex_hourly %>%
              mutate(name = 'Excess') %>%
              select(case,year,policy, Time_index, name, value)
            temp_ex_total <- temp_ex_hourly %>%
              mutate(Weight = temp_timeweight$Weight) %>%
              group_by(case, year, policy,name) %>%
              summarize(value = sum(Weight*value)) %>%
              select(case,year,policy, name, value)
            
            temp_sf_hourly <- temp_sf_ex_calculation %>%
              mutate(value = value.y-value.x)
            temp_sf_hourly$value[which(temp_sf_hourly$value <0)] <-0
            temp_sf_hourly <- temp_sf_hourly %>%
              mutate(name = 'Shortfall') %>%
              select(case,year,policy, Time_index, name, value)   
            temp_sf_total <- temp_sf_hourly %>%
              mutate(Weight = temp_timeweight$Weight) %>%
              group_by(case, year, policy, name) %>%
              summarize(value = sum(Weight*value)) %>%
              select(case,year,policy, name, value)
          }
          cfe_ts_allcase_cfe = rbind(cfe_ts_allcase_cfe, temp_cfe_hourly)
          cfe_ts_allcase_cfe = rbind(cfe_ts_allcase_cfe, temp_ex_hourly)
          cfe_ts_allcase_cfe = rbind(cfe_ts_allcase_cfe, temp_sf_hourly)
        }

        
        # Calculate preliminaries
        storage_rows = which(temp_generator[, storage_col] >= 1)
        dr_rows = which(temp_generator[,dr_col] >=1)
        cfe_rows = which(temp_generator[,policy_col] > 0)


        clean_power_rows = which(temp_generator$All_Fuel %in% clean_fuel)
        if (Studyregion == 'PJM'){
          local_rows = grep('PJM_', temp_generator$region)
        }
        if (Studyregion == 'WECC'){
          local_rows = grep('CA_', temp_generator$region)
        }
        
        local_injection_rows = setdiff(local_rows, dr_rows) # Local gen + storage
        rest_of_local_injection_rows = setdiff(local_injection_rows, cfe_rows) # Local gen + storage but not 24x7
        local_clean_injection_rows = intersect(local_injection_rows, clean_power_rows) # Local clean gen, no storage because they will be automatically excluded because clean_fuel does not include 
        rest_of_local_clean_injection_rows = setdiff(local_clean_injection_rows, cfe_rows) # Local clean gen but not 24x7, no storage because they will be automatically excluded because clean_fuel does not include
        local_storage_rows = intersect(local_rows, storage_rows) # Local storage
        local_dr_rows = intersect(local_rows, dr_rows) # Local DR
        rest_of_system_injection_rows = setdiff(which(temp_generator[,dr_col] ==0), local_injection_rows) # non-local non-DR generators, not the local ones
        rest_of_system_clean_injection_rows = intersect(rest_of_system_injection_rows, clean_power_rows) # non-local non-DR clean generators
        
        local_power = rowSums(power_ts[,local_injection_rows]) # total injection into CA/PJM
        rest_of_local_power = rowSums(power_ts[,rest_of_local_injection_rows]) # total injection into CA/PJM excluding 24x7
        rest_of_system_power = rowSums(power_ts[,rest_of_system_injection_rows]) # total injection into restofWECC/neighboring regions of PJM
        
        local_load_afterdr = local_load + rowSums(power_ts[,local_dr_rows] - charge_ts[,local_dr_rows])
        local_withdraw = local_load_afterdr + rowSums(charge_ts[,local_storage_rows])
        
        net_interchange = (local_withdraw - local_power)
        net_import = net_interchange
        net_import[which(net_import <0)] <- 0
        net_export = (-1)*net_interchange
        net_export[which(net_export <0)] <- 0
        
        # California/PJM hourly clean power
        local_hourly_clean_power = rowSums(power_ts[,local_clean_injection_rows])
        # California/PJM hourly cleanliness
        local_hourly_cleanness = local_hourly_clean_power/local_power        
        # rest of California/PJM clean power
        rest_of_local_hourly_clean_power <- rowSums(power_ts[,rest_of_local_clean_injection_rows])
        # rest of California/PJM cleanliness
        rest_of_local_hourly_cleanness = rest_of_local_hourly_clean_power/rest_of_local_power
        # rest of EI/WECC clean power
        rest_of_system_hourly_clean_power <- rowSums(power_ts[,rest_of_system_clean_injection_rows])
        # rest of EI/WECC cleanliness
        rest_of_system_hourly_cleanness = rest_of_system_hourly_clean_power/rest_of_system_power
        net_imported_cleanpower = net_import*rest_of_system_hourly_cleanness
        rest_of_local_plus_import_hourly_cleanness = ((net_imported_cleanpower + rest_of_local_hourly_clean_power)/
                                                        (net_import + rest_of_local_power))
        local_plus_import_hourly_cleanness = ((net_imported_cleanpower + local_hourly_clean_power)/
                                                (net_import + local_power))
        # calculate SF*Cleanness
        if (!grepl('nocip',cases[i])){ #annual 100 and nocip
          sf = temp_sf_hourly$value
          sf_clean_local = sum(sf*rest_of_local_hourly_cleanness*temp_timeweight$Weight)
          sf_clean_local_n_import = sum(sf*rest_of_local_plus_import_hourly_cleanness*temp_timeweight$Weight)

          denominator = (temp_loss$value[1] + cfe_annualload$value[1])
          totalcfe_local = (temp_cfe_total$value[1]- temp_ex_total$value[1] + sf_clean_local)
          totalcfe_local_n_import = (temp_cfe_total$value[1]- temp_ex_total$value[1] + sf_clean_local_n_import)
          expost_cfe_score_local = totalcfe_local/denominator
          expost_cfe_score_local_n_import = totalcfe_local_n_import/denominator
          modified_cfe_score = (temp_cfe_total$value[1] - temp_ex_total$value[1])/denominator
          cfeload_cleanness_local = (sum(cfe_modified_load$value *local_hourly_cleanness*temp_timeweight$Weight))/denominator
          cfeload_cleanness_local_n_import = (sum(cfe_modified_load$value*local_plus_import_hourly_cleanness*temp_timeweight$Weight))/denominator
          cfe_table_row = as_tibble_row(c(`case` = cases[i],
                                          `year` = years[j],
                                          `Policy` = paste0('RPSH_',k),
                                          `Shortfall` = round(temp_sf_total$value[1]/1e6,3),
                                          `Shortfall price` = as.numeric(temp_sf_shadowprice),
                                          `Excess` = round(temp_ex_total$value[1]/1e6,3),
                                          `Excess price` = as.numeric(temp_ex_shadowprice),
                                          `Load` = round(cfe_annualload$value[1]/1e6,3),
                                          `Storage loss` = round(temp_loss$value[1]/1e6,3),
                                          `Pre-Grid CFE Score` = round(modified_cfe_score,3),
                                          `Post-Grid CFE Score Local` = round(expost_cfe_score_local,3),
                                          `Post-Grid CFE Score Local_n_Import` = round(expost_cfe_score_local_n_import,3),
                                          `Local Cleanness` = round(cfeload_cleanness_local,3),
                                          `Local_n_Import Cleanness` = round(cfeload_cleanness_local_n_import,3)))
        } else { # no cip
          sf = cfe_load_afterdr$value # all of them are from grid supply
          sf_clean_local = sum(sf*rest_of_local_hourly_cleanness*temp_timeweight$Weight)
          sf_clean_local_n_import = sum(sf*rest_of_local_plus_import_hourly_cleanness*temp_timeweight$Weight)
          denominator = (temp_loss$value[1] + cfe_annualload$value[1])
          expost_cfe_score_local = sf_clean_local/denominator
          expost_cfe_score_local_n_import = sf_clean_local_n_import/denominator
          modified_cfe_score = 0
          cfeload_cleanness_local = (sum(sf *local_hourly_cleanness*temp_timeweight$Weight))/denominator
          cfeload_cleanness_local_n_import = (sum(sf*local_plus_import_hourly_cleanness*temp_timeweight$Weight))/denominator
          cfe_table_row = as_tibble_row(c(`case` = cases[i],
                                          `year` = years[j],
                                          `Policy` = paste0('RPSH_',k),
                                          `Shortfall` = 0,
                                          `Shortfall price` = 0,
                                          `Excess` = 0,
                                          `Excess price` = 0,
                                          `Load` = round(cfe_annualload$value[1]/1e6,3),
                                          `Storage loss` = round(temp_loss$value[1]/1e6,3),
                                          `Pre-Grid CFE Score` = round(modified_cfe_score,3),
                                          `Post-Grid CFE Score Local` = round(expost_cfe_score_local,3),
                                          `Post-Grid CFE Score Local_n_Import` = round(expost_cfe_score_local_n_import,3),
                                          `Local Cleanness` = round(cfeload_cleanness_local,3),
                                          `Local_n_Import Cleanness` = round(cfeload_cleanness_local_n_import,3)))
        }
        cfe_table = rbind(cfe_table,cfe_table_row)
        temp_hourly_cleaness = as_tibble(cbind(case = rep(cases[i],n_hour),
                                               year = rep(years[j],n_hour),
                                               Time_index = c(1:n_hour),
                                               `Rest of Local Cleanness` = rest_of_local_hourly_cleanness,
                                               `Rest of Local and Import Cleanness` = rest_of_local_plus_import_hourly_cleanness,
                                               `Local Cleanness` = local_hourly_cleanness,
                                               `Local and Import Cleanness` = local_plus_import_hourly_cleanness))
        cfe_hourly_cleanness = rbind(cfe_hourly_cleanness,temp_hourly_cleaness)
        
        
        
        # calculate emissions in CFE (ccs) 
        cfe_ccs_rows = intersect(cfe_rows, which(temp_generator$CO2_Capture_Rate > 0))
        emission_captured = read_csv(temp_emission_captured_fn, 
                                     col_types = cols(),
                                     col_names = F,
                                     skip = 6)[,-1]
        cfe_emissions_hourly = rowSums((emission_captured[,cfe_ccs_rows]/
                                          t(temp_generator$CO2_Capture_Rate[cfe_ccs_rows])*
                                          t(1-temp_generator$CO2_Capture_Rate[cfe_ccs_rows])))
        if (length(cfe_emissions_hourly) == 0) {
          cfe_emissions_hourly = rep(0, n_hour)
        }
        
        cfe_emissions_row = 0
        cfe_emissions_row = cfe_emissions_hourly %*% temp_timeweight$Weight; 
        # calcualte local/restoflocal/restofsystem emissions
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
        system_emission_hourly = rowSums(temp_zonal_emission) #hourly emission of WECC/EI
        rest_of_system_emission_hourly = system_emission_hourly - local_emission_hourly
        # rest of EI/WECC emissions
        rest_of_system_emission_rate_hourly = rest_of_system_emission_hourly/rest_of_system_power
        net_imported_emission = net_import*rest_of_system_emission_rate_hourly
        # Emission rate calculation
        # if (!grepl('nocip',cases[i])){
        #   rest_of_local_emission_rate_hourly = rest_of_local_emission_hourly/(rest_of_local_power + temp_ex_hourly$value)
        #   rest_of_local_plus_import_emission_rate_hourly = (net_imported_emission + rest_of_local_emission_hourly)/(rest_of_local_power + net_import + temp_ex_hourly$value)
        # }
        rest_of_local_emission_rate_hourly = rest_of_local_emission_hourly/rest_of_local_power
        rest_of_local_plus_import_emission_rate_hourly = (net_imported_emission + rest_of_local_emission_hourly)/(rest_of_local_power + net_import)
        local_emission_rate_hourly = local_emission_hourly/local_power
        local_plus_import_emission_rate_hourly = (local_emission_hourly + net_imported_emission)/(local_power + net_import)
        
        shortfallemission_local_row = 0
        shortfallemission_system_row = 0
        if (!grepl('nocip',cases[i])) {
          shortfallemission_local_row = t(temp_sf_hourly$value * rest_of_local_emission_rate_hourly) %*% temp_timeweight$Weight
          shortfallemission_system_row = t(temp_sf_hourly$value * rest_of_local_plus_import_emission_rate_hourly) %*% temp_timeweight$Weight
        }
        emission_local_row = t(cfe_modified_load$value * local_emission_rate_hourly) %*% temp_timeweight$Weight
        emission_system_row = t(cfe_modified_load$value * local_plus_import_emission_rate_hourly) %*% temp_timeweight$Weight
        temp_emission_measure = emission_system_row
        if (!grepl('nocip',cases[i])) {
          temp_emission_measure = shortfallemission_system_row + cfe_emissions_row
        }
        temp_row = as_tibble_row(c(
          case = cases[i],
          year = years[j],
          cfeemission = cfe_emissions_row,
          shortfallemission_local = shortfallemission_local_row, 
          shortfallemission_local_n_import = shortfallemission_system_row,
          emission_local = emission_local_row,
          emission_local_n_import = emission_system_row,
          emission_measure = temp_emission_measure,
          `Participated Load` = cfe_annualload$value[1],
          `Storage Loss` = temp_loss$value[1])
        )
        cfe_emission_table = rbind(cfe_emission_table, temp_row)
        
        emission_local_all_row = t(local_withdraw * local_emission_rate_hourly) %*% temp_timeweight$Weight
        # Update on Nov 4
        rest_of_local_emission_w_import = t((local_withdraw-cfe_modified_load$value) * rest_of_local_plus_import_emission_rate_hourly) %*% temp_timeweight$Weight
        rest_of_local_emission = t((local_withdraw-cfe_modified_load$value) * rest_of_local_emission_rate_hourly) %*% temp_timeweight$Weight
        # this is the emission that the load is responsible
        emission_system_all_row = t(local_withdraw * local_plus_import_emission_rate_hourly) %*% temp_timeweight$Weight
        local_load_row = t(local_withdraw) %*% temp_timeweight$Weight
        temp_row = as_tibble_row(c(
          case = cases[i],
          year = years[j],
          emission_local = emission_local_all_row,
          emission_local_n_import = emission_system_all_row,
          emission_rest_of_local = rest_of_local_emission,
          emission_rest_of_local_w_import = rest_of_local_emission_w_import,
          participating_load_emission = temp_emission_measure,
          `Local Load` = local_load_row)
        )
        cfe_system_emission_table = rbind(cfe_system_emission_table, temp_row)
        
        temp_matrix = as_tibble(cbind(
          case = rep(cases[i],n_hour),
          year = rep(years[j],n_hour),
          Time_index = c(1:n_hour),
          `Rest of Local Emission Rate` = rest_of_local_emission_rate_hourly,
          `Rest of Local and Import Emission Rate` = rest_of_local_plus_import_emission_rate_hourly,
          `Local Emission Rate` = local_emission_rate_hourly,
          `Local and Import Emission Rate` =local_plus_import_emission_rate_hourly,
          `Total Withdraw` = local_withdraw
        ))
        cfe_hourly_emission = rbind(cfe_hourly_emission, temp_matrix)
      }
    }
  }
  print(i)
  print(Sys.time())
}
print(Sys.time())

cfe_table_withname <- cfe_table %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(cfe_table_withname, paste0(temp_RunFdr,"/CompiledResults/CFE_table.csv"))

cfe_ts_allcase = rbind(cfe_ts_allcase_gen, 
                       cfe_ts_allcase_cfe,
                       cfe_ts_allcase_load_dr_stor)
cfe_ts_allcase_withname <- cfe_ts_allcase %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(cfe_ts_allcase_withname, paste0(temp_RunFdr,"/CompiledResults/CFE_timeseries.csv"))

cfe_hourly_cleanness_withname <- cfe_hourly_cleanness %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(cfe_hourly_cleanness_withname, paste0(temp_RunFdr,"/CompiledResults/CFE_cleanness_timeseries.csv"))

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
cfe_ts_allcase_withname <- read_csv(paste0(temp_RunFdr,"/CompiledResults/CFE_timeseries.csv"),
                                    col_types = cols()) %>%
  pivot_wider(names_from = 'name',values_from = 'value')
cfe_ts_allcase_withname[is.na(cfe_ts_allcase_withname)] = 0;

cfe_hourly_cleanness_withname <- read_csv(paste0(temp_RunFdr,"/CompiledResults/CFE_cleanness_timeseries.csv")) %>%
  select(case,year,Time_index,Scenario,TechSensitivity,
         `Rest of Local Cleanness`,`Rest of Local and Import Cleanness`,
         `Local Cleanness`, `Local and Import Cleanness`)

cfe_ts_load_columns = which(colnames(cfe_ts_allcase_withname) %in% 
                              c('case','year','policy','Time_index','Scenario','TechSensitivity',
                                'Load','Storage Discharge','Storage Charge','DR Load Increase', 'DR Load Decrease','Modified Load'))
cfe_ts_load = cfe_ts_allcase_withname[,cfe_ts_load_columns]
cfe_ts_cfe_columns = which(colnames(cfe_ts_allcase_withname) %in% 
                             c('case','year','policy','Time_index','Scenario','TechSensitivity',
                               'CFE','Excess','Shortfall'))
cfe_ts_cfe = cfe_ts_allcase_withname[,cfe_ts_cfe_columns]

cfe_ts_gen_columns = which(colnames(cfe_ts_allcase_withname) %in% 
                             c('case','year','policy','Time_index','Scenario','TechSensitivity',
                               'Gas CC w/CCS','Nuclear','Geothermal','Offshore Wind',
                               'Onshore Wind','Retrofit Gas CC w/CCS','Utility Solar','ZCF CC',
                               'Adv. Nuclear','Geothermal NFEGS'))
cfe_ts_gen = cfe_ts_allcase_withname[,cfe_ts_gen_columns]

cfe_hourly_emission_withname <- read_csv(paste0(temp_RunFdr,'/CompiledResults/system_emissions_hourly.csv')) %>%
  select(case,year,Time_index,Scenario,TechSensitivity,
         `Rest of Local Emission Rate`,`Rest of Local and Import Emission Rate`,
         `Local Emission Rate`, `Local and Import Emission Rate`,`Total Withdraw`)


if(exists('cfe_ts_load_full')) { rm(cfe_ts_load_full)}
if(exists('cfe_ts_cfe_full')) { rm(cfe_ts_cfe_full)}
if(exists('cfe_ts_gen_full')) { rm(cfe_ts_gen_full)}
if(exists('ts_cleanness_full')) {rm(ts_cleanness_full)}
if(exists('cfe_hourly_emission_full')) {rm(cfe_hourly_emission_full)}

for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_load_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Load_data.csv");
    temp_ts_mapping_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/time_series_mapping.csv");
    temp_ts_load = cfe_ts_load %>% filter(case == cases[i], year == years[j])
    temp_ts_cfe = cfe_ts_cfe %>% filter(case == cases[i], year == years[j])
    temp_ts_gen = cfe_ts_gen %>% filter(case == cases[i], year == years[j])
    temp_ts_cleanness = cfe_hourly_cleanness_withname %>% filter(case == cases[i], year == years[j])
    temp_ts_emission = cfe_hourly_emission_withname %>% filter(case == cases[i], year == years[j])
    if(nrow(temp_ts_cfe) >0){
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
        temp_ts_load_full <- left_join(template, temp_ts_load, by = c('Time_index')) %>%
          select(-c(Slot, cluster, Month, Time_index))
        temp_ts_cfe_full <- left_join(template, temp_ts_cfe, by = c('Time_index')) %>%
          select(-c(Slot, cluster, Month, Time_index))
        temp_ts_gen_full <- left_join(template, temp_ts_gen, by = c('Time_index')) %>%
          select(-c(Slot, cluster, Month, Time_index))
        temp_ts_cleanness_full <- left_join(template, temp_ts_cleanness, by = c('Time_index')) %>%
          select(-c(Slot, cluster, Month, Time_index))
        temp_ts_emission_full <- left_join(template, temp_ts_emission, by = c('Time_index')) %>%
          select(-c(Slot, cluster, Month, Time_index))
      }
      if(!exists('cfe_ts_load_full')){
        cfe_ts_load_full <- temp_ts_load_full;
        cfe_ts_cfe_full <- temp_ts_cfe_full
        cfe_ts_gen_full  <- temp_ts_gen_full
        ts_cleanness_full <- temp_ts_cleanness_full
        cfe_hourly_emission_full <- temp_ts_emission_full;
      } else {
        cfe_ts_load_full <- rbind(cfe_ts_load_full, temp_ts_load_full)
        cfe_ts_cfe_full <- rbind(cfe_ts_cfe_full, temp_ts_cfe_full)
        cfe_ts_gen_full <- rbind(cfe_ts_gen_full,temp_ts_gen_full)
        ts_cleanness_full <- rbind(ts_cleanness_full, temp_ts_cleanness_full)
        cfe_hourly_emission_full <- rbind(cfe_hourly_emission_full, temp_ts_emission_full)
      }
      print(i)
    }
  }
}
write_csv(cfe_ts_load_full, paste0(temp_RunFdr,"/CompiledResults/CFE_load_timeseries_full.csv"))
write_csv(cfe_ts_cfe_full, paste0(temp_RunFdr,"/CompiledResults/CFE_cfe_timeseries_full.csv"))
write_csv(cfe_ts_gen_full, paste0(temp_RunFdr,"/CompiledResults/CFE_gen_timeseries_full.csv"))
write_csv(ts_cleanness_full, paste0(temp_RunFdr,"/CompiledResults/CFE_cleanness_timeseries_full.csv"))
write_csv(cfe_hourly_emission_full, paste0(temp_RunFdr,"/CompiledResults/CFE_emissionrate_timeseries_full.csv"))


