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
    temp_hourcfeprice_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                               years[j],"_",cases[i],"/Results/RPSH_Hourlymatchingdual.csv");
    #Note the hourly cfe prices are with timeweights.
    temp_sf_shadowprice_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                  years[j],"_",cases[i],"/Results/RPSH_SFlimitdual.csv");
    temp_ex_shadowprice_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                  years[j],"_",cases[i],"/Results/RPSH_EXlimitdual.csv");
    
    if (file.exists(temp_cfe_fn)){
      temp_sf_shadowprice <- read_csv(temp_sf_shadowprice_fn, col_types = cols())
      temp_ex_shadowprice <- read_csv(temp_ex_shadowprice_fn, col_types = cols())

      # Calculate the hourly local demand, for the calculation of the import
      temp_demand <- read_csv(temp_load_fn, col_types = cols()) %>%
        select(-c(1:9))
      if (Studyregion == 'PJM'){
        local_load = rowSums(temp_demand[,c(5:13)])
       }
      if (Studyregion == 'WECC'){
        local_load = rowSums(temp_demand[,c(1:2)])
      }
      rm(temp_demand)
      temp_timeweight <- read_csv(temp_timeweight_fn, col_types = cols())
      temp_hourcfeprice <- read_csv(temp_hourcfeprice_fn, col_types = cols())      
      temp_generator <- read_csv(temp_generator_fn, col_types = cols()) %>%
        left_join(resource_mapping_includingflexibleload, by = c('Resource' = 'All_Resource'))
      # CFE without subtracting the excess
      temp_cfe <- t(read_csv(temp_cfe_fn, col_types = cols()));
      n_cfe <- dim(temp_cfe)[2]
      n_hour <- dim(temp_cfe)[1]

      colnames(temp_cfe) <- paste0('RPSH_',c(1:n_cfe))
      rownames(temp_cfe) <- paste0(c(1:n_hour))
      
      temp_cfe_total <- as_tibble(temp_cfe) %>%
        mutate(Weight = temp_timeweight$Weight,
               case = cases[i],
               year = years[j]) %>%
        pivot_longer(cols = !c(Weight, case, year)) %>%
        group_by(name) %>%
        summarize(CFE = sum(Weight*value))
      
      temp_cfe_hourly <- as_tibble(temp_cfe) %>%
        mutate(case = cases[i],
               year = years[j],
               Time_index = c(1:n_hour),
               name = 'CFE') %>%
        pivot_longer(cols = !c(case, year,Time_index,name),names_to = 'policy') %>%
        select(case,year,policy, Time_index,name, value)
      
      cfe_ts_allcase_cfe = rbind(cfe_ts_allcase_cfe, temp_cfe_hourly)
      # Excess
      temp_ex <- t(read_csv(temp_ex_fn, col_types = cols()));
      colnames(temp_ex) <- paste0('RPSH_',c(1:n_cfe))
      rownames(temp_ex) <- paste0(c(1:n_hour))
      temp_ex_total <- as_tibble(temp_ex) %>%
        mutate(Weight = temp_timeweight$Weight,
               case = cases[i],
               year = years[j]) %>%
        pivot_longer(cols = !c(Weight, case, year)) %>%
        group_by(name) %>%
        summarize(EX = sum(Weight*value))
      temp_ex_hourly <- as_tibble(temp_ex) %>%
        mutate(case = cases[i],
               year = years[j],
               Time_index = c(1:n_hour),
               name = 'Excess') %>%
        pivot_longer(cols = !c(case, year,Time_index,name),names_to = 'policy') %>%
        select(case,year,policy, Time_index,name, value)
      cfe_ts_allcase_cfe = rbind(cfe_ts_allcase_cfe, temp_ex_hourly)
      
      # Shortfall
      # Step 1: read in the shortfall and format
      temp_sf <- t(read_csv(temp_sf_fn, col_types = cols()));
      colnames(temp_sf) <- paste0('RPSH_',c(1:n_cfe))
      rownames(temp_sf) <- paste0(c(1:n_hour))
      # Step 2: read in the dirtiness
      if (!file.exists(temp_sf_dirtiness_fn)){
        temp_sf_dirtiness = as_tibble_col(c(rep(1,n_hour))) %>%
          rename(RPSH_SFDT_1 = value)
      } else {
        temp_sf_dirtiness <- read_csv(temp_sf_dirtiness_fn, col_types = cols())[,-1];
      }
      
      # Step 3: calculate the hourly shortfall dirtiness
      # They should be in the same dimension
      temp_sf_dirtiness_hourly <- temp_sf*temp_sf_dirtiness
      # Step 4: calculate the annual total
      temp_sf_total <- as_tibble(temp_sf_dirtiness_hourly) %>%
        mutate(Weight = temp_timeweight$Weight,
               case = cases[i],
               year = years[j]) %>%
        pivot_longer(cols = !c(Weight, case, year)) %>%
        group_by(name) %>%
        summarize(SF = sum(Weight*value))
      

      temp_sf_hourly <- as_tibble(temp_sf) %>%
        mutate(case = cases[i],
               year = years[j],
               Time_index = c(1:n_hour),
               name = 'Shortfall') %>%
        pivot_longer(cols = !c(case, year,Time_index,name), names_to = 'policy') %>%
        select(case,year,policy, Time_index,name, value)
      cfe_ts_allcase_cfe = rbind(cfe_ts_allcase_cfe, temp_sf_hourly)
      
      
      temp_cfeload_all <- read_csv(temp_cfeload_fn, col_types = cols())
      temp_cfepolicy <- read_csv(temp_cfepolicy_fn, col_types = cols())
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

      
      for (k in 1:n_cfe){
        member_col = which(colnames(temp_cfepolicy)==paste0('RPSH_MEMBER_',k))
        sf_limit_col = which(colnames(temp_cfepolicy)==paste0('RPSH_SFLIMIT_',k))
        ex_limit_col = which(colnames(temp_cfepolicy)==paste0('RPSH_EXLIMIT_',k))
        
        cfe_zones = which(temp_cfepolicy[,member_col] == 1)
        cfe_load = rowSums(temp_cfeload_all[,cfe_zones+1])
        cfe_annualload = as_tibble(cfe_load) %>%
          mutate(Weight = temp_timeweight$Weight,
                 case = cases[i],
                 year = years[j]) %>%
          group_by(case, year) %>%
          summarize(Load = sum(Weight*value))
        
        # cfe_annualload = as_tibble(temp_cfeload_all[,cfe_zones+1]) %>%
        #   mutate(Weight = temp_timeweight$Weight,
        #          case = cases[i],
        #          year = years[j]) %>%
        #   pivot_longer(!c(case,year,Weight)) %>%
        #   group_by(case,year,name)%>%
        #   summarize(Load = sum(Weight*value))
          
        cfe_load_hourly = as_tibble(cfe_load) %>%
          mutate(case = cases[i],
                 year = years[j],
                 policy = paste0('RPSH_',k),
                 Time_index = c(1:n_hour),
                 name = 'Load') %>%
          select(case,year,policy, Time_index,name, value)
        
        cfe_ts_allcase_load_dr_stor = rbind(cfe_ts_allcase_load_dr_stor, cfe_load_hourly)
        
        # calculate hourly dr, and storage

        policy_col = which(colnames(temp_generator)==paste0('RPSH_',k))
        storage_col = which(colnames(temp_generator)==paste0('STOR'))
        dr_col = which(colnames(temp_generator)==paste0('DR'))
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
        
        # figure out which rows are storage
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
        # to save the memory, remove the following data
        rm(cfe_load_hourly, cfe_storage_ts_discharge, cfe_storage_ts_charge, 
           cfe_dr_loadincrease,cfe_dr_loaddecrease)
        
        
        
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
        
        # Calculate hourly cleanness
        storage_rows = which(temp_generator[, storage_col] >= 1)
        dr_rows = which(temp_generator[,dr_col] >=1)
        cfe_rows = which(temp_generator[,policy_col] >0)
        clean_power_rows = which(temp_generator$All_Fuel %in% clean_fuel)
        if (Studyregion == 'PJM'){
          local_rows = grep('PJM_', temp_generator$region)
        }
        if (Studyregion == 'WECC'){
          local_rows = grep('CA_', temp_generator$region)
        }        
        local_injection_rows = setdiff(local_rows, dr_rows) # Local gen + storage
        rest_of_local_injection_rows = setdiff(local_injection_rows, cfe_rows)
        local_clean_injection_rows = intersect(local_injection_rows, clean_power_rows)
        rest_of_local_clean_injection_rows = setdiff(local_clean_injection_rows, cfe_rows)
        local_withdraw_rows = intersect(local_rows, storage_rows)# Local storage
        local_dr_rows = intersect(local_rows, dr_rows)
        rest_of_system_injection_rows = setdiff(which(temp_generator[,dr_col] ==0), local_injection_rows)
        rest_of_system_clean_injection_rows = intersect(rest_of_system_injection_rows, clean_power_rows)
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
        net_export[which(net_export <0)] <- 0
        
        # Note: Identify the the columns of power_ts that is renewable
        # local_clean_power_rows = intersect(local_gen_rows, clean_power_rows)
        # rest_of_local_clean_gen_rows = intersect(rest_of_local_gen_rows, clean_power_rows)
        # rest_of_system_clean_gen_rows = intersect(rest_of_system_gen_rows, clean_power_rows)
        
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
        sf = temp_sf[,k]
        sf_clean_local = sum(sf*rest_of_local_hourly_cleanness*temp_timeweight$Weight)
        sf_clean_local_n_import = sum(sf*rest_of_local_plus_import_hourly_cleanness*temp_timeweight$Weight)
        
        # calculate the expost cfe score
        totalcfe_local = (temp_cfe_total$CFE[temp_cfe_total$name == paste0('RPSH_',k)] - 
          temp_ex_total$EX[temp_ex_total$name == paste0('RPSH_',k)] +
          sf_clean_local)
        totalcfe_local_n_import = (temp_cfe_total$CFE[temp_cfe_total$name == paste0('RPSH_',k)] - 
                            temp_ex_total$EX[temp_ex_total$name == paste0('RPSH_',k)] +
                              sf_clean_local_n_import)
        expost_cfe_score_local = totalcfe_local/(temp_loss + cfe_annualload$Load)
        expost_cfe_score_local_n_import = totalcfe_local_n_import/(temp_loss + cfe_annualload$Load)
        modified_cfe_score = ((temp_cfe_total$CFE[temp_cfe_total$name == paste0('RPSH_',k)] - 
                                temp_ex_total$EX[temp_ex_total$name == paste0('RPSH_',k)])/
          (temp_loss + cfe_annualload$Load))
        
        cfeload_cleanness_local = (sum(cfe_modified_load$value *local_hourly_cleanness*temp_timeweight$Weight))/(cfe_annualload$Load + temp_loss)
        cfeload_cleanness_local_n_import = (sum(cfe_modified_load$value*local_plus_import_hourly_cleanness*temp_timeweight$Weight))/(cfe_annualload$Load + temp_loss)
        
        cfe_table_row = as_tibble_row(c(`case` = cases[i],
                                        `year` = years[j],
                                        `Policy` = paste0('RPSH_',k),
                                        `Shortfall` = round(temp_sf_total$SF[temp_sf_total$name == paste0('RPSH_SFDT_',k)]/1e6,3),
                                        `Shortfall price` = as.numeric(temp_sf_shadowprice[k,1]),
                                        `Excess` = round(temp_ex_total$EX[temp_ex_total$name == paste0('RPSH_',k)]/1e6,3),
                                        `Excess price` = as.numeric(temp_ex_shadowprice[k,1]),
                                        `Load` = round(cfe_annualload$Load/1e6,3),
                                        `Storage loss` = round(temp_loss/1e6,3),
                                        `Pre-Grid CFE Score` = round(modified_cfe_score,3),
                                        `Post-Grid CFE Score Local` = round(expost_cfe_score_local,3),
                                        `Post-Grid CFE Score Local_n_Import` = round(expost_cfe_score_local_n_import,3),
                                        `Local Cleanness` = round(cfeload_cleanness_local,3),
                                        `Local_n_Import Cleanness` = round(cfeload_cleanness_local_n_import,3)))
        cfe_table = rbind(cfe_table,cfe_table_row)
        temp_hourly_cleaness = as_tibble(cbind(case = rep(cases[i],n_hour),
                                               year = rep(years[j],n_hour),
                                               Time_index = c(1:n_hour),
                                               `Rest of Local Cleanness` = rest_of_local_hourly_cleanness,
                                               `Rest of Local and Import Cleanness` = rest_of_local_plus_import_hourly_cleanness,
                                               `Local Cleanness` = local_hourly_cleanness,
                                               `Local and Import Cleanness` = local_plus_import_hourly_cleanness))
        cfe_hourly_cleanness = rbind(cfe_hourly_cleanness,temp_hourly_cleaness)
        
        
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
                               'Geothermal EFEGS', 'Adv. Nuclear'))
cfe_ts_gen = cfe_ts_allcase_withname[,cfe_ts_gen_columns]

if(exists('cfe_ts_load_full')) { rm(cfe_ts_load_full)}
if(exists('cfe_ts_cfe_full')) { rm(cfe_ts_cfe_full)}
if(exists('cfe_ts_gen_full')) { rm(cfe_ts_gen_full)}
if(exists('ts_cleanness_full')) {rm(ts_cleanness_full)}


for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_load_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Load_data.csv");
    temp_ts_mapping_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/time_series_mapping.csv");
    temp_ts_load = cfe_ts_load %>% filter(case == cases[i], year == years[j])
    temp_ts_cfe = cfe_ts_cfe %>% filter(case == cases[i], year == years[j])
    temp_ts_gen = cfe_ts_gen %>% filter(case == cases[i], year == years[j])
    temp_ts_cleanness = cfe_hourly_cleanness_withname %>% filter(case == cases[i], year == years[j])
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
      }
      if(!exists('cfe_ts_load_full')){
        cfe_ts_load_full <- temp_ts_load_full;
        cfe_ts_cfe_full <- temp_ts_cfe_full
        cfe_ts_gen_full  <- temp_ts_gen_full
        ts_cleanness_full <- temp_ts_cleanness_full
      } else {
        cfe_ts_load_full <- rbind(cfe_ts_load_full, temp_ts_load_full)
        cfe_ts_cfe_full <- rbind(cfe_ts_cfe_full, temp_ts_cfe_full)
        cfe_ts_gen_full <- rbind(cfe_ts_gen_full,temp_ts_gen_full)
        ts_cleanness_full <- rbind(ts_cleanness_full, temp_ts_cleanness_full)
      }
      print(i)
    }
  }
}
write_csv(cfe_ts_load_full, paste0(temp_RunFdr,"/CompiledResults/CFE_load_timeseries_full.csv"))
write_csv(cfe_ts_cfe_full, paste0(temp_RunFdr,"/CompiledResults/CFE_cfe_timeseries_full.csv"))
write_csv(cfe_ts_gen_full, paste0(temp_RunFdr,"/CompiledResults/CFE_gen_timeseries_full.csv"))
write_csv(ts_cleanness_full, paste0(temp_RunFdr,"/CompiledResults/CFE_cleanness_timeseries_full.csv"))



