
#---------------------------------------#
# Combining Gen Settlement data ----
#---------------------------------------#
if (exists('Settlement')){
  rm('Settlement','Regional_Settlement','Settlement_Short')
}
print('begin compiling generation settlement')
print(Sys.time())
# tracker = 0
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_settlement_fn <- paste0(RunFdr,"/",years[j],"/",
                                 case_ids[i],"_",years[j],"_",
                                 cases[i],"/Results/NetRevenue.csv");
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],
                                "/Inputs/Generators_data.csv");
    temp_capacity_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                               years[j],"_",cases[i],"/Results/capacity.csv");
    temp_power_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],
                            "_",cases[i],"/Results/power.csv");
    temp_charge_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Results/charge.csv");
    if (file.exists(temp_settlement_fn)) {
      temp_settlement = read_csv(temp_settlement_fn, col_types = cols());
      temp_capacity <- read_csv(temp_capacity_fn, col_types = cols()) %>%
        filter(Zone != 'n/a') %>%
        select(EndCap,EndEnergyCap,EndChargeCap,
               NewCap,NewEnergyCap,NewChargeCap,
               RetCap,RetEnergyCap,RetChargeCap)
      temp_settlement = cbind(temp_settlement,temp_capacity)
      
      temp_power = t(read_csv(temp_power_fn, 
                              col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_power) <- temp_power[1,] # make the row one as column name
      temp_power <- as_tibble(temp_power[-c(1, dim(temp_power)[1]),]) %>%
        select(AnnualSum)%>%
        rename(DischargeSum = AnnualSum)
      temp_settlement = cbind(temp_settlement,temp_power)
      
      temp_charge = t(read_csv(temp_charge_fn, 
                               col_names = F, n_max = 3, col_types = cols()));
      colnames(temp_charge) <- temp_charge[1,] # make the row one as column name
      temp_charge <- as_tibble(temp_charge[-c(1, dim(temp_charge)[1]),]) %>%
        select(AnnualSum) %>%
        rename(ChargeSum = AnnualSum)
      temp_settlement = cbind(temp_settlement,temp_charge)
      
      t_generators <- read_csv(temp_generator_fn, col_types = cols()) %>%
        select(Fuel)
      temp_settlement = cbind(temp_settlement,t_generators)
      

      if (!('EmissionsCapture' %in% colnames(temp_settlement))){
        temp_settlement$EmissionsCapture = 0
      }
      temp_settlement$case = cases[i]
      temp_settlement$year = years[j]
    }
    # tracker = tracker + 1
    # print(tracker)
    if(!exists('Settlement'))
    {
      Settlement <- temp_settlement;
    }
    else
    {
      Settlement <- rbind(Settlement, temp_settlement);
    }
  }
}


if (exists('Settlement')){
  
  Settlement <- Settlement %>% 
    rename(Region = region, Zone = zone);
  
  Settlement <- Settlement %>%
    mutate(Region = as.factor(Region),
           year = as.factor(year),
           case = as.factor(case),
           Resource = as.factor(Resource),
           Cluster = as.factor(Cluster),
           Zone = as.factor(Zone),
           Fuel = as.factor(Fuel),
           Var_OM_cost_out = -1*Var_OM_cost_out,
           Var_OM_cost_in = -1*Var_OM_cost_in,
           Inv_cost_MW = -1*Inv_cost_MW,
           Inv_cost_MWh = -1*Inv_cost_MWh,
           Fixed_OM_cost_MWh = -1*Fixed_OM_cost_MWh,
           Fixed_OM_cost_MW = -1*Fixed_OM_cost_MW,
           Fuel_cost = -1*Fuel_cost,
           Charge_cost = -1*Charge_cost,
           EmissionsCost = -1*EmissionsCost,
           EmissionsCapture = -1*EmissionsCapture,
           StartCost = -1*StartCost) %>%
    mutate(VOM_n_Fuel = Var_OM_cost_out + Var_OM_cost_in + 
             Fuel_cost + StartCost,
           FOM = Fixed_OM_cost_MWh + Fixed_OM_cost_MW,
           Inv_cost = Inv_cost_MW + Inv_cost_MWh)

  # if (identical(years, c('2030','2040','2050'))){
  #   source('./code/misc/ZCF Cost Calculation.R')
  # }
  ###########################
  # Calculate sunk cost ----
  ###########################
  # SunkCost <- subset(Settlement,select = c("Region","Resource","Fuel","Zone","Cluster","case","year")) %>%
  #   mutate(SunkCost = 0);
  Settlement$SunkCost = 0;
  if (length(years) >1){
    for (i in 1:length(cases)){
      for (j in 2:length(years)){ # The first year is assumed to have zero sunk cost.
        settlement_treatment <- Settlement %>% 
          filter(case == cases[i],year == years[j]) %>%
          select(-SunkCost);
        settlement_previous_year <- Settlement %>% 
          filter(case == cases[i],year == years[j-1]) %>%
          select(Region, Resource, Zone, Cluster,Inv_cost,SunkCost) %>%
          mutate(SunkCost = Inv_cost + SunkCost) %>%
          select(-Inv_cost);
        settlement_rest <- Settlement %>% filter((case != cases[i])|(year != years[j]));
        settlement_treatment <- left_join(settlement_treatment, 
                                          settlement_previous_year,
                                          by = c("Region","Resource","Zone","Cluster"))
        Settlement <- rbind(settlement_rest, settlement_treatment);
        rm(settlement_rest, settlement_treatment,settlement_previous_year)
      }
    }
  }
  # # Special  treatment for battery in the PJM study.
  # if (identical(years, c('2030','2040','2050'))){
  #   settlement_battery_modification <- Settlement %>% 
  #     filter(year == '2030') %>%
  #     select(Region, Resource, Zone, Cluster,Inv_cost,case) %>%
  #     rename(bat_mod = Inv_cost)
  #   non_battery_row <- which(!grepl('battery_*', settlement_battery_modification$Resource))
  #   settlement_battery_modification$bat_mod[non_battery_row] <- 0;
  #   Settlement <- left_join(Settlement, settlement_battery_modification, 
  #                           by = c("Region","Resource","Zone","Cluster","case"));
  #   battery_row_2050 <- which((grepl('battery_*',Settlement$Resource))&(Settlement$year == '2050'))
  #   Settlement$SunkCost[battery_row_2050] <- Settlement$SunkCost[battery_row_2050] - Settlement$bat_mod[battery_row_2050]
  #   Settlement <- Settlement %>% select(-bat_mod)
  #   
  #   Settlement_temp1 <- subset(Settlement,Fuel == "ZCF") %>%
  #     mutate(Resource = paste(Resource,"_ZCF",sep = ""));
  #   Settlement <- rbind(Settlement_temp1, subset(Settlement,Fuel != "ZCF"));
  #   
  # }
  
  Settlement <- subset(Settlement,select = -c(Fuel));
  
  Settlement_Short <- subset(Settlement, 
                             select = c("Region","Resource","case","year","Zone","Cluster",
                                        "EnergyRevenue","SubsidyRevenue","RegSubsidyRevenue",
                                        "ReserveMarginRevenue", "ESRRevenue","VOM_n_Fuel",
                                        "FOM","Inv_cost", "SunkCost","Charge_cost",
                                        "EmissionsCost","EmissionsCapture","EndCap",
                                        "EndEnergyCap","EndChargeCap","DischargeSum","ChargeSum"));
  write_csv(Settlement, paste0(RunFdr,"/CompiledResults/Settlement.csv"));
  write_csv(Settlement_Short, paste0(RunFdr,"/CompiledResults/Settlement_short.csv"));
  print('finished compiling generation settlement')
  print(Sys.time())
} else {
  print('there are no NetRevenue.csv files')
  print(Sys.time())
}
