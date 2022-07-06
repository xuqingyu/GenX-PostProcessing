# source('./code/Header.R')
# Compile Different LSE payment cost into one table
# Energy Payment ----
# Note that Flexible load's cost is considered

lse_energy_base_fn <- paste0(RunFdr,'/CompiledResults/LSEEnergyPayment.csv');
if (file.exists(lse_energy_base_fn)) {
  lse_energy_base <- read_csv(lse_energy_base_fn, col_types = cols()) %>%
    rename(`Energy Payment Base` = AnnualSum) %>%
    select(case, year, Zone, `Energy Payment Base`) %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone))
} else {
  print('ERROR: there is no LSEEnergyPayment.csv')
}

gen_settlement_fn <- paste0(RunFdr,'/CompiledResults/Settlement_short.csv');
if (file.exists(gen_settlement_fn)) {
  lse_energy_flexible_load <- read_csv(gen_settlement_fn, col_types = cols()) %>%
    filter(Resource %in% flexiload_list) %>%
    group_by(case, year, Region) %>%
    summarize(`Energy Payment Flexible Load` = sum(EnergyRevenue + Charge_cost)) %>% #for now, Energy Revenue for flexible load is in reverse.
    left_join(zone_mapping,by = c("Region" = "region")) %>%
    rename(Zone = zone) %>%
    select(case, year, Zone, `Energy Payment Flexible Load`);
} else {
  print('ERROR: there is no Settlement_short.csv')
}

if (exists('lse_energy_base')) {
  if (exists('lse_energy_flexible_load')) {
    lse_energy <- left_join(lse_energy_base,lse_energy_flexible_load, col_types = cols());
    lse_energy[is.na(lse_energy)] <- 0 # it is possible that there is no flexible load resource, to enable that the summation will work, we need to change NA to zero
    lse_energy <- lse_energy %>%
      mutate(`Energy Payment` = `Energy Payment Base` + `Energy Payment Flexible Load`)
  } else {
    print('ERROR: there is no lse_energy_flexible_load')
  }
} else {
  print('ERROR: there is no lse_energy_base')
}

# NSE
nse_fn <- paste0(RunFdr,'/CompiledResults/LSENSECost.csv');
if (file.exists(nse_fn)) {
  lse_nse <- read_csv(nse_fn, col_types = cols()) %>%
    rename(`NSE Cost` = value) %>%
    select(case, year, Zone, `NSE Cost`) %>%
    mutate(Zone = factor(Zone , levels = zone_mapping$zone))
  
} else {
  print('ERROR: there is no LSENSECost.csv')
}
if (exists('lse_nse')) {
  lse_payment_notrans <- left_join(lse_energy, lse_nse);
} else {
  print('ERROR: there is no lse_nse')
}




# Transmission Loss
lse_transloss_fn <- paste0(RunFdr,'/CompiledResults/LSETransLossCost.csv');
if (file.exists(lse_transloss_fn)) {
  lse_transloss <- read_csv(lse_transloss_fn, col_types = cols()) %>%
    rename(`Transmission Loss Cost` = AnnualSum) %>%
    select(case, year, Zone, `Transmission Loss Cost`) %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone))
  if (exists('lse_transloss')){
    lse_payment_notrans <- left_join(lse_payment_notrans, lse_transloss)
  } else {
    print('there is no lse_transloss')
    lse_payment_notrans$`Transmission Loss Cost` <- 0
  }
} else {
  print('There is no LSETransLossCost.csv')
  lse_payment_notrans$`Transmission Loss Cost` <- 0
}


# Capacity Payment ----
lse_capacity_all_fn <- paste0(RunFdr,'/CompiledResults/LSECapacityPayment.csv');
if (file.exists(lse_capacity_all_fn)) {
  lse_capacity_all <- read_csv(lse_capacity_all_fn, col_types = cols()) %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone))
  
  lse_capacity_base <- lse_capacity_all %>%
    filter(grepl('payment', item)) %>%
    group_by(case, year, Zone) %>%
    summarize(`Capacity Payment Base` = sum(value))
  lse_capacity_load_management <- lse_capacity_all %>%
    filter(grepl('demandreponsesaving', item)) %>%
    group_by(case, year, Zone) %>%
    summarize(`Capacity Revenue Load Management` = sum(value))
  lse_capacity <- left_join(lse_capacity_base, lse_capacity_load_management);
  gen_settlement_fn <- paste0(RunFdr,'/CompiledResults/Settlement_short.csv');
  if (file.exists(gen_settlement_fn)) {
    lse_capacity_flexible_load <- read_csv(paste0(RunFdr,'/CompiledResults/Settlement_short.csv'), 
                                           col_types = cols()) %>%
      mutate(Zone = factor(Zone, levels = zone_mapping$zone)) %>%
      filter(Resource %in% flexiload_list) %>%
      group_by(case,year, Zone) %>%
      summarize(`Capacity Revenue Flexible Load` = (-1)*sum(ReserveMarginRevenue)) %>%
      select(case, year, Zone, `Capacity Revenue Flexible Load`)
    lse_capacity <- left_join(lse_capacity, lse_capacity_flexible_load)
  } else {
    print('ERROR: there is no Settlement_short.csv')
  }
  lse_capacity[is.na(lse_capacity)] <- 0
  lse_capacity <- lse_capacity%>%
    mutate(`Capacity Payment` = `Capacity Payment Base` + `Capacity Revenue Load Management` + `Capacity Revenue Flexible Load`)
  if (exists('lse_capacity')) {
    lse_payment_notrans <- left_join(lse_payment_notrans, lse_capacity)
  } else {
    print('there is no lse_capacity')
    lse_payment_notrans$`Capacity Payment` <- 0
  }
} else {
  print('ERROR: there is no LSECapacityPayment.csv')
  lse_payment_notrans$`Capacity Payment` <- 0
}


# CO2 (mass and load based) ----
lse_co2_mass_fn <- paste0(RunFdr,'/CompiledResults/LSECO2Revenue_mass.csv');
if (file.exists(lse_co2_mass_fn)) {
  lse_co2_mass <- read_csv(lse_co2_mass_fn, col_types = cols()) %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone)) %>% 
    group_by(case, year, Zone) %>%
    summarize(`CO2 Revenue Mass Cap` = (-1) * sum(value))
  if (exists('lse_co2_mass')) {
    lse_payment_notrans <- left_join(lse_payment_notrans, lse_co2_mass)
  } else { 
    print('there is no lse_co2_mass')
    lse_payment_notrans$`CO2 Revenue Mass Cap` <- 0
  }
} else {
  print('there is no LSECO2Revenue_mass.csv')
  lse_payment_notrans$`CO2 Revenue Mass Cap` <- 0
}


lse_co2_loadrate_fn <- paste0(RunFdr,'/CompiledResults/LSECO2Revenue_loadrate.csv');
if (file.exists(lse_co2_loadrate_fn)){
  lse_co2_loadrate <- read_csv(lse_co2_loadrate_fn, col_types = cols()) %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone)) %>%
    filter(!grepl('StorageLoss',item)) %>%
    group_by(case, year, Zone) %>%
    summarize(`CO2 Revenue Load Rate Cap` = (-1) * sum(value))
  if (exists('lse_co2_loadrate')) {
    lse_payment_notrans <- left_join(lse_payment_notrans, lse_co2_loadrate)
  } else { 
    print('there is no lse_co2_loadrate')
    lse_payment_notrans$`CO2 Revenue Load Rate Cap` <- 0
  }
} else {
  print('there is no LSECO2Revenue_loadrate.csv')
  lse_payment_notrans$`CO2 Revenue Load Rate Cap` <- 0
}



lse_co2_tax_fn <- paste0(RunFdr,'/CompiledResults/LSECO2Revenue_tax.csv');
if (file.exists(lse_co2_tax_fn)){
  lse_co2_tax <- read_csv(lse_co2_tax_fn) %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone)) %>% 
    group_by(case, year, Zone) %>%
    summarize(`CO2 Revenue Tax` = (-1) * sum(value))
  if (exists('lse_co2_tax')) {
    lse_payment_notrans <- left_join(lse_payment_notrans, lse_co2_tax)
  } else {
    print('there is no lse_co2_tax')
    lse_payment_notrans$`CO2 Revenue Tax` <- 0
  }
} else {
  print('there is no LSECO2Revenue_tax.csv')
  lse_payment_notrans$`CO2 Revenue Tax` <- 0
}


# RPS ----
lse_rps_all_fn <- paste0(RunFdr,'/CompiledResults/LSERPSPayment.csv')
lse_rps_trans_fn <- paste0(RunFdr,'/CompiledResults/ESRpaymentTransmissionloss.csv')
if (file.exists(lse_rps_all_fn)) {
  lse_rps_all <- read_csv(lse_rps_all_fn, col_types = cols()) %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone))
  if (file.exists(lse_rps_trans_fn)) {
    lse_rps_trans <- read_csv(lse_rps_trans_fn, col_types = cols()) %>%
      mutate(Zone = factor(Zone, levels = zone_mapping$zone))
    lse_rps_all = rbind(lse_rps_all, lse_rps_trans)
  }
  lse_rps_load <- lse_rps_all %>%
    filter(grepl('esrpayment|esrpaymenttransloss', item)) %>%
    group_by(case, year, Zone) %>%
    summarize(`RPS Payment` = sum(value))
  lse_rps <- lse_rps_load %>%
    mutate(`RPS Total Payment` = `RPS Payment`)
  if (exists('lse_rps')) {
    lse_payment_notrans <- left_join(lse_payment_notrans, lse_rps)
  } else {
    print('there is no lse_rps')
    lse_payment_notrans$`RPS Total Payment` <- 0
  }
} else {
  print('there is no LSERPSPayment.csv')
  lse_payment_notrans$`RPS Total Payment` <- 0
}



# Subsidy ----
gen_settlement_fn <- paste0(RunFdr,'/CompiledResults/Settlement_short.csv');
if (file.exists(gen_settlement_fn)) {
  lse_subsidy <- read_csv(paste0(RunFdr,'/CompiledResults/Settlement_short.csv'),
                          col_types = cols()) %>%
    group_by(case,year,Zone) %>%
    summarize(`Tech Subsidy Cost` = sum(SubsidyRevenue + RegSubsidyRevenue)) %>%
    # left_join(zone_mapping,by = c("Region" = "region")) %>%
    select(case, year, Zone, `Tech Subsidy Cost`) %>%
    mutate(Zone = factor(Zone, levels = zone_mapping$zone))
  if (exists('lse_subsidy')) {
    lse_payment_notrans <- left_join(lse_payment_notrans, lse_subsidy)
  } else {
    print('there is no lse_subsidy')
    lse_payment_notrans$`Tech Subsidy Cost` <- 0
  }
} else {
  print('there is Settlement_short.csv')
  lse_payment_notrans$`Tech Subsidy Cost` <- 0
}

lse_payment_notrans <- lse_payment_notrans %>%
  select(case,year,Zone, `Energy Payment`, `NSE Cost`,
         `Transmission Loss Cost`, `Capacity Payment`,
         `CO2 Revenue Mass Cap`,`CO2 Revenue Load Rate Cap`,
         `CO2 Revenue Tax`,`RPS Total Payment`,`Tech Subsidy Cost`)
lse_payment_notrans[is.na(lse_payment_notrans)] <- 0;

# reading the load ----
temp_load <- read_csv(paste0(RunFdr,'/CompiledResults/Total_load_summary.csv'), 
                      col_types = cols()) %>%
  rename(Zone = Region) %>%
  mutate(Zone = str_remove(Zone,"Load_MW_z")) %>%
  mutate(Zone = factor(Zone, levels = zone_mapping$zone))
lse_payment_notrans <- left_join(lse_payment_notrans, temp_load) %>%
  mutate(AnnualLoad = AnnualLoad*1e6) # change the unit to MWh
write_csv(lse_payment_notrans,paste0(RunFdr,'/CompiledResults/LSE_Payment_NoTrans.csv'))


