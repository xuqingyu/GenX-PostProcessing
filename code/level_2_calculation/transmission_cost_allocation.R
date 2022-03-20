
# Read in Transmission Allocation ----
lse_payment_notrans <- read_csv(paste0(RunFdr,'/CompiledResults/LSE_Payment_NoTrans.csv'),
                                col_types = cols()) %>%
  mutate(Zone = factor(Zone, levels = zone_mapping$zone)) %>%
  left_join(rto_mapping, by = c('Zone'='zone')) %>%
  select(-c(loadzone)) %>%
  rename(SystemOperator = System_membership)
system_trans_value_fn <- paste0(RunFdr,'/CompiledResults/Trans_Value.csv');
if (file.exists(system_trans_value_fn)){
  system_trans_value <- read_csv(system_trans_value_fn,
                                 col_types = cols()) %>%
    select(case, year, SystemOperator,`Transmission Cost Per MWh`,`Congestion Revenue Per MWh`)
  lse_payment <- left_join(lse_payment_notrans, system_trans_value)%>%
    na.omit() %>%
    mutate(`Transmission Cost` = `Transmission Cost Per MWh` * AnnualLoad,
           `Congestion Revenue` = `Congestion Revenue Per MWh` * AnnualLoad) %>%
    select(-c(`Transmission Cost Per MWh`, `Congestion Revenue Per MWh`, `SystemOperator`));
  lse_payment_result <- lse_payment %>%
    select(case,year,Zone, AnnualLoad, `Energy Payment`, `NSE Cost`,
           `Transmission Loss Cost`, 
           `Capacity Payment`, 
           `CO2 Revenue Mass Cap`, 
           `CO2 Revenue Load Rate Cap`,
           `CO2 Revenue Tax`, 
           `RPS Total Payment`,
           `Tech Subsidy Cost`, 
           `Transmission Cost`,
           `Congestion Revenue`) %>%
  write_csv(paste0(RunFdr,'/CompiledResults/LSE_Payment.csv'))
}

