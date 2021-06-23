if(exists('trans_exp_cost_allocated')){
  rm(trans_exp_cost, case_year_list, rto_list,temp_trans_exp_cost,trans_exp_cost_allocated)
}
year_levels <- c('start', as.character(years_pre$Model_years));
years_pre$Model_years <- factor(years_pre$Model_years, levels = year_levels)
years_pre$Pre_years <- factor(years_pre$Pre_years, levels = year_levels)
if (zone_count>1){
  trans_exp_cost <- read_csv(paste0(RunFdr,'/CompiledResults/trans.csv')) %>% select(Line,Cost_Trans_Capacity,case,year)
  trans_exp_cost$Line <- as.factor(trans_exp_cost$Line)
  trans_exp_cost$year <- factor(trans_exp_cost$year,levels = year_levels)
  case_year_list <- unique(select(trans_exp_cost, case, year))
  rto_list <- unique(trans_cost_mapping$SystemOperatorName)
  for (i in (1:dim(case_year_list)[1])){
    temp_trans_exp_cost <- trans_exp_cost %>% filter(trans_exp_cost$case == case_year_list$case[i],
                                                     trans_exp_cost$year == case_year_list$year[i])
    if (length(rto_list>0)){
      temp_trans_exp_cost <- temp_trans_exp_cost %>% 
        left_join(trans_cost_mapping, by = c('Line' = 'Network_lines')) %>%
        group_by(case,year,SystemOperatorName) %>%
        summarize(`Incremental Transmission Cost` = sum(Cost_Trans_Capacity*SystemOperatorShare))
    }
    if (!exists('trans_exp_cost_allocated')){
      trans_exp_cost_allocated <- temp_trans_exp_cost;
    } else {
      trans_exp_cost_allocated <- rbind(trans_exp_cost_allocated,temp_trans_exp_cost)
    }
  }
  trans_exp_cost_allocated <- trans_exp_cost_allocated %>%
    rename(SystemOperator = SystemOperatorName)
  if(exists('trans_existing_cost_allocated')){
    rm(trans_existing_cost_allocated, temp_trans_existing_cost)
  }
  for (j in unique(case_year_list$case)){
    for (k in (1:length(years_pre$Model_years))){
      temp_trans_existing_cost <- as_tibble_col(rto_list,column_name = "SystemOperator");
      temp_trans_existing_cost$case <- j;
      temp_trans_existing_cost$year <- years_pre$Model_years[k];
      if (years_pre$Pre_years[k]=='start'){
        temp_trans_existing_cost <- temp_trans_existing_cost %>% 
          left_join(trans_cost_startcost) %>%
          rename(`Existing Transmission Cost` = ExistingTransmissionCost);
      } else {
        temp_trans_existing_cost <- trans_existing_cost_allocated %>% 
          filter(case == j, year == years_pre$Pre_years[k]) %>%
          left_join(trans_exp_cost_allocated) %>%
          mutate(`Existing Transmission Cost` = `Existing Transmission Cost` + `Incremental Transmission Cost`) %>%
          select(-`Incremental Transmission Cost`) %>%
          mutate(year = years_pre$Model_years[k])
      }
      if (!exists('trans_existing_cost_allocated')){
        trans_existing_cost_allocated <- temp_trans_existing_cost;
      } else {
        trans_existing_cost_allocated <- rbind(trans_existing_cost_allocated,temp_trans_existing_cost)
      }
    }
  }
  TransmissionCostTable = left_join(trans_exp_cost_allocated, trans_existing_cost_allocated) %>%
    mutate(`Total Transmission Cost` = `Incremental Transmission Cost` + `Existing Transmission Cost`)
  
  # Congestion Revenue --- 
  congestion_rev <- read_csv(paste0(RunFdr,'/CompiledResults/TransCongestionRevenue.csv')) %>% select(Line,Sum,case,year)
  congestion_rev$Line <- as.factor(congestion_rev$Line)
  congestion_rev$year <- factor(congestion_rev$year,levels = year_levels)
  case_year_list <- unique(select(congestion_rev, case, year))
  rto_list <- unique(trans_cost_mapping$SystemOperatorName)
  for (i in (1:dim(case_year_list)[1])){
    temp_congestion_rev <- congestion_rev %>% filter(congestion_rev$case == case_year_list$case[i],
                                                     congestion_rev$year == case_year_list$year[i])
    if (length(rto_list>0)){
      temp_congestion_rev <- temp_congestion_rev %>% 
        left_join(trans_cost_mapping, by = c('Line' = 'Network_lines')) %>%
        group_by(case,year,SystemOperatorName) %>%
        summarize(`Congestion Revenue` = (-1)*sum(Sum*SystemOperatorShare))
    }
    if (!exists('congestion_rev_allocated')){
      congestion_rev_allocated <- temp_congestion_rev;
    } else {
      congestion_rev_allocated <- rbind(congestion_rev_allocated,temp_congestion_rev)
    }
  }
  congestion_rev_allocated <- congestion_rev_allocated %>%
    rename(SystemOperator = SystemOperatorName)
  
  # Read in load ----
  temp_load <- read_csv(paste0(RunFdr,'/CompiledResults/Total_load_summary.csv')) %>%
    rename(loadzone = Region) %>%
    left_join(rto_mapping) %>%
    na.omit()%>% 
    group_by(case,year,System_membership) %>%
    summarize(AnnualLoad = sum(AnnualLoad)) %>%
    rename(SystemOperator = System_membership)
  temp_load$year <- factor(temp_load$year, levels = year_levels)
  # Merge load with Transmission Cost Table
  TransmissionCostTable_All <- TransmissionCostTable %>% 
    left_join(temp_load) %>%
    mutate(`Transmission Cost Per MWh` = `Total Transmission Cost`/AnnualLoad/1e6)
  # Merge load with Congestion Revenue Table 
  CongestionRevenueTable_All <- congestion_rev_allocated %>% 
    left_join(temp_load) %>%
    mutate(`Congestion Revenue Per MWh` = `Congestion Revenue`/AnnualLoad/1e6)
  # Merge Transmission Cost with Congestion Revenue
  Trans_Value_Table <- left_join(TransmissionCostTable_All,CongestionRevenueTable_All)
  write_csv(Trans_Value_Table,paste0(RunFdr,'/CompiledResults/Trans_Value.csv'))
}