
#---------------------------------------#
# Combining Gen Settlement data ----
#---------------------------------------#
if (exists('Settlement')){
  rm('Settlement','Regional_Settlement','Settlement_Short')
}
print('begin compiling generation settlement')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_settlement_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/NetRevenue.csv");
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Generators_data.csv");    
    if (file.exists(temp_settlement_fn)) {
      temp_settlement = read_csv(temp_settlement_fn);
      t_generators <- read_csv(temp_generator_fn);
      temp_settlement$Fuel <- t_generators$Fuel
      if (!('EmissionsCapture' %in% colnames(temp_settlement))){
        temp_settlement$EmissionsCapture = 0
      }
      temp_settlement <- aggregate(cbind(
        Var_OM_cost_out	,
        Var_OM_cost_in,
        Fixed_OM_cost_MW,
        Fixed_OM_cost_MWh,
        Fuel_cost,
        Charge_cost,
        Inv_cost_MW,	
        Inv_cost_MWh,
        StartCost,
        EmissionsCost,
        EmissionsCapture,
        EnergyRevenue,
        SubsidyRevenue,
        RegSubsidyRevenue,
        ReserveMarginRevenue,
        RPSRevenue
      )~region+Resource+Fuel+zone+Cluster,temp_settlement,sum)
      temp_settlement$case = cases[i]
      temp_settlement$year = years[j]
    }
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
  
  capacity_for_settlement <- read_csv(paste0(RunFdr,"/CompiledResults/capacity_for_settlement.csv")) %>%
    mutate(year = as.factor(year));
  Settlement <- left_join(Settlement,capacity_for_settlement,by = c("Region","Resource","Fuel", "Zone","Cluster","case","year"));
  
  power_for_settlement <- read_csv(paste0(RunFdr,"/CompiledResults/power_for_settlement.csv")) %>%
    mutate(year = as.factor(year));
  Settlement <- left_join(Settlement,power_for_settlement,by = c("Region","Resource","Fuel", "Zone","Cluster", "case","year"))
  
  charge_for_settlement <- read_csv(paste0(RunFdr,"/CompiledResults/charge_for_settlement.csv")) %>%
    mutate(year = as.factor(year)) %>%
    rename(ChargeSum = Sum);  
  Settlement <- left_join(Settlement,charge_for_settlement,by = c("Region","Resource","Fuel", "Zone","Cluster", "case","year"))
  
  
  
  Settlement$Region <- as.factor(Settlement$Region);
  Settlement$year <- as.factor(Settlement$year);
  Settlement$case <- as.factor(Settlement$case);
  Settlement$Resource <- as.factor(Settlement$Resource);
  Settlement$Cluster <- as.factor(Settlement$Cluster);
  Settlement$Zone <- as.factor(Settlement$Zone);
  Settlement$Fuel <- as.factor(Settlement$Fuel);
  Settlement$Var_OM_cost_out = -1*Settlement$Var_OM_cost_out;
  Settlement$Var_OM_cost_in = -1*Settlement$Var_OM_cost_in;
  Settlement$Inv_cost_MW = -1*Settlement$Inv_cost_MW;
  Settlement$Inv_cost_MWh = -1*Settlement$Inv_cost_MWh;
  Settlement$Fixed_OM_cost_MWh = -1*Settlement$Fixed_OM_cost_MWh;
  Settlement$Fixed_OM_cost_MW = -1*Settlement$Fixed_OM_cost_MW;
  Settlement$Fuel_cost = -1*Settlement$Fuel_cost;
  Settlement$Charge_cost = -1*Settlement$Charge_cost;
  Settlement$EmissionsCost = -1*Settlement$EmissionsCost;
  Settlement$EmissionsCapture = -1*Settlement$EmissionsCapture;
  Settlement$StartCost = -1*Settlement$StartCost;
  Settlement$VOM_n_Fuel <- Settlement$Var_OM_cost_out+Settlement$Var_OM_cost_in+Settlement$Fuel_cost+Settlement$StartCost;
  Settlement$FOM <- Settlement$Fixed_OM_cost_MWh + Settlement$Fixed_OM_cost_MW;
  Settlement$Inv_cost <- Settlement$Inv_cost_MW + Settlement$Inv_cost_MWh;
  # Calculate repower cost and separate it from FOM
  if (identical(years, c('2030','2040','2050'))){
    source('./code/misc/ZCF Cost Calculation.R')
  }
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
        settlement_treatment <- left_join(settlement_treatment, settlement_previous_year,by = c("Region","Resource","Zone","Cluster"))
        Settlement <- rbind(settlement_rest, settlement_treatment);
        rm(settlement_rest, settlement_treatment,settlement_previous_year)
      }
    }
  }
  # Special  treatment for battery in the PJM study.
  if (identical(years, c('2030','2040','2050'))){
    settlement_battery_modification <- Settlement %>% 
      filter(year == '2030') %>%
      select(Region, Resource, Zone, Cluster,Inv_cost,case) %>%
      rename(bat_mod = Inv_cost)
    non_battery_row <- which(!grepl('battery_*',settlement_battery_modification$Resource))
    settlement_battery_modification$bat_mod[non_battery_row] <- 0;
    Settlement <- left_join(Settlement, settlement_battery_modification, by = c("Region","Resource","Zone","Cluster","case"));
    battery_row_2050 <- which((grepl('battery_*',Settlement$Resource))|(Settlement$year == '2050'))
    Settlement$SunkCost[battery_row_2050] <- Settlement$SunkCost[battery_row_2050] - Settlement$bat_mod[battery_row_2050]
    Settlement <- Settlement %>% select(-bat_mod)
  }
  
  Settlement_temp1 <- subset(Settlement,Fuel == "ZCF") %>%
    mutate(Resource = paste(Resource,"_ZCF",sep = ""));
  Settlement <- rbind(Settlement_temp1, subset(Settlement,Fuel != "ZCF"));
  Settlement <- subset(Settlement,select = -c(Fuel));
  
  Settlement_Short <- subset(Settlement, select = c("Region","Resource","case","year","Zone","Cluster",
                                                    "EnergyRevenue","SubsidyRevenue","RegSubsidyRevenue",
                                                    "ReserveMarginRevenue",
                                                    "RPSRevenue","VOM_n_Fuel","FOM","Inv_cost",
                                                    "SunkCost","Charge_cost","EmissionsCost","EmissionsCapture","EndCap","EndEnergyCap","Sum","ChargeSum"));
  write_csv(Settlement, paste0(RunFdr,"/CompiledResults/Settlement.csv"));
  write_csv(Settlement_Short, paste0(RunFdr,"/CompiledResults/Settlement_short.csv"));
  print('finished compiling generation settlement')
} else {
  print('there are no NetRevenue.csv files')
}