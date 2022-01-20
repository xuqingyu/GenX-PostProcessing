# C&I load energy payment ----
# Created by Qingyu Xu
# Created on Sept 7, 2021

# Energy Cost ----
ci_loadcost = tibble(
  case = character(),
  year = character(),
  `Energy Payment` = numeric(),
  `Participated Load` = numeric()
)
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_timeweight_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Results/time_weights.csv");
    temp_cfeload_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                              years[j],"_",cases[i],"/Inputs/Load_data_RPSH.csv");
    temp_generator_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_charge_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Results/charge.csv");
    temp_power_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                            years[j],"_",cases[i],"/Results/power.csv");
    temp_energyprice_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                  years[j],"_",cases[i],"/Results/prices_w.csv");
    temp_energyrevenue_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                  years[j],"_",cases[i],"/Results/EnergyRevenue_Kai.csv");
    temp_chargingcost_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                    years[j],"_",cases[i],"/Results/ChargingCost_Kai.csv");
    
    temp_timeweight <- read_csv(temp_timeweight_fn, col_types = cols())
    
    # Energy payment of original load ----
    temp_cfeload_all <- read_csv(temp_cfeload_fn, col_types = cols())[,-1]
    temp_energyprice <- read_csv(temp_energyprice_fn, col_types = cols())[,-1]
    temp_payment = sum(t(temp_cfeload_all * temp_energyprice) %*% temp_timeweight$Weight)
    ci_load = sum(t(temp_cfeload_all)%*% temp_timeweight$Weight)
    # Energy Revenue of DR
    temp_generator <- read_csv(temp_generator_fn, col_types = cols())
    if (grepl('_5ci',cases[i])) {
      CIPurchaseRatio = 0.05
    } else if (grepl('_10ci',cases[i])){
      CIPurchaseRatio = 0.10
    } else if (grepl('_25ci',cases[i])){
      CIPurchaseRatio = 0.25
    }
    if (Studyregion == 'PJM') {
      dr_rows = which((temp_generator$DR >=1) & 
                        (grepl('commercial',temp_generator$Resource)) & 
                        (grepl('PJM_',temp_generator$region)))
    } else if (Studyregion == 'WECC') {
      dr_rows = which((temp_generator$DR >=1) & 
                        (grepl('commercial',temp_generator$Resource)) & 
                        (grepl('CA_' ,temp_generator$region)))
    }
    temp_dr_energyrevenue <- sum(read_csv(temp_energyrevenue_fn, col_types = cols())$Sum[dr_rows])*CIPurchaseRatio
    temp_dr_energypayment <- sum(read_csv(temp_chargingcost_fn, col_types = cols())$Sum[dr_rows])*CIPurchaseRatio
    temp_energypayment = temp_payment - temp_dr_energyrevenue + temp_dr_energypayment
    temp_row = as_tibble_row(c(case = cases[i], year = years[j], 
                               `Energy Payment` = as.numeric(temp_energypayment),
                               `Participated Load` = as.numeric(ci_load)))
    ci_loadcost = rbind(ci_loadcost,temp_row)
  }
}
ci_loadcost <- ci_loadcost %>%
  mutate(`Energy Payment` = as.numeric(`Energy Payment`),
         `Participated Load` = as.numeric(`Participated Load`))
# ci_loadcost_withname <- left_join(ci_loadcost, cases_newnames, by = c('case' = 'case_description')) %>%
#   select(case,year,Scenario, TechSensitivity, `Energy Payment`,`Participated Load`) %>%
#   mutate(`Energy Payment ($/MWh)` = as.numeric(`Energy Payment`)/as.numeric(`Participated Load`))
# write_csv(ci_loadcost_withname, paste0(temp_RunFdr,"/CompiledResults/ci_energycost.csv"))


# Capacity Payment----
# The capacity payment will be calculated using 
# do it per CapRes Price

ci_capacitycost = tibble(
  case = character(),
  year = character(),
  `Capacity Payment` = numeric(),
  `Participated Load` = numeric()
)
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_timeweight_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Results/time_weights.csv");
    temp_cfeload_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                              years[j],"_",cases[i],"/Inputs/Load_data_RPSH.csv");
    temp_generator_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_capres_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/CapRes.csv");
    temp_capacityprice_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                  years[j],"_",cases[i],"/Results/ReserveMargin_w.csv");
    temp_capacityrevenue_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                    years[j],"_",cases[i],"/Results/ReserveMarginRevenue.csv");
    temp_timeweight <- read_csv(temp_timeweight_fn, col_types = cols())
    
    # Energy payment of original load ----
    temp_cfeload_all <- read_csv(temp_cfeload_fn, col_types = cols())[,-1] %>%
      as.matrix()
    temp_capres <- read_csv(temp_capres_fn, col_types = cols())[,-c(1,2)] %>% 
      as.matrix() 
    temp_capacityprice <- read_csv(temp_capacityprice_fn, col_types = cols())[,-1]
    
    temp_payment = sum(t((temp_cfeload_all %*% (1+temp_capres)) * temp_capacityprice) %*% temp_timeweight$Weight)
    ci_load = sum(t(temp_cfeload_all)%*% temp_timeweight$Weight)
    
    # Energy Revenue of DR
    temp_generator <- read_csv(temp_generator_fn, col_types = cols())
    if (grepl('_5ci',cases[i])) {
      CIPurchaseRatio = 0.05
    } else if (grepl('_10ci',cases[i])){
      CIPurchaseRatio = 0.10
    } else if (grepl('_25ci',cases[i])){
      CIPurchaseRatio = 0.25
    }
    if (Studyregion == 'PJM') {
      dr_rows = which((temp_generator$DR >=1) & 
                        (grepl('commercial',temp_generator$Resource)) & 
                        (grepl('PJM_',temp_generator$region)))
    } else if (Studyregion == 'WECC') {
      dr_rows = which((temp_generator$DR >=1) & 
                        (grepl('commercial',temp_generator$Resource)) & 
                        (grepl('CA_' ,temp_generator$region)))
    }
    temp_dr_capacityrevenue <- sum(read_csv(temp_capacityrevenue_fn, col_types = cols())$Sum[dr_rows])*CIPurchaseRatio
    temp_capacitypayment = temp_payment - temp_dr_capacityrevenue
    temp_row = as_tibble_row(c(case = cases[i], year = years[j], 
                               `Capacity Payment` = temp_capacitypayment,
                               `Participated Load` = ci_load))
    ci_capacitycost = rbind(ci_capacitycost,temp_row)
  }
}
ci_capacitycost <- ci_capacitycost %>%
  mutate(`Capacity Payment` = as.numeric(`Capacity Payment`),
         `Participated Load` = as.numeric(`Participated Load`))
# ci_capacitycost_withname <- left_join(ci_capacitycost, cases_newnames, by = c('case' = 'case_description')) %>%
#   select(case,year,Scenario, TechSensitivity, `Capacity Payment`,`Participated Load`) %>%
#   mutate(`Capacity Payment ($/MWh)` = as.numeric(`Capacity Payment`)/as.numeric(`Participated Load`))
# write_csv(ci_capacitycost_withname, paste0(temp_RunFdr,"/CompiledResults/ci_capacitycost.csv"))

# Transmission cost ----
# use the allocated transmission cost 
# For CA, no existing transmission is added.
ci_load_allcase = select(ci_loadcost, c(case,year,`Participated Load`)) %>%
  mutate(`Participated Load` = as.numeric(`Participated Load`))

trans_allocatedcost <- read_csv(paste0(temp_RunFdr, "/CompiledResults/",subreg,
                                       "/Load/LSE_Payment_",subreg,"_with2019_and_DG.csv"),
                                col_types = cols()) %>%
  select(case,year,AnnualLoad,`Transmission Cost`,`Congestion Revenue`) %>%
  mutate(year = as.character(year)) %>%
  left_join(ci_load_allcase) %>%
  mutate(Ratio = `Participated Load`/AnnualLoad) %>%
  mutate(`Transmission Cost` = `Transmission Cost`*Ratio,
         `Congestion Revenue` = `Congestion Revenue`*Ratio) %>%
  select(case,year, `Transmission Cost`,`Congestion Revenue`)

# Emission cost ----
# for California, tax is gathered to the state level and allocated per MWh grossload
# for PJM, revenue is gathered to the state level and allocated per MWh grossload

emissionrevenue <- read_csv(paste0(temp_RunFdr, "/CompiledResults/",subreg,
                                   "/Load/LSE_Payment_",subreg,"_with2019_and_DG.csv"),
                            col_types = cols()) %>%
  select(case,year,`Gross Total`,`CO2 Revenue`) %>%
  mutate(year = as.character(year)) %>%
  left_join(ci_load_allcase) %>%
  mutate(Ratio = `Participated Load`/`Gross Total`) %>%
  mutate(`CO2 Revenue` = `CO2 Revenue`*Ratio) %>%
  select(case,year, `CO2 Revenue`)

# Tech Specific cost ----

techsubsidycost <- read_csv(paste0(temp_RunFdr, "/CompiledResults/",subreg,
                                   "/Load/LSE_Payment_",subreg,"_with2019_and_DG.csv"),
                            col_types = cols()) %>%
  select(case,year,`Gross Total`,`Tech Subsidy Cost`) %>%
  mutate(year = as.character(year)) %>%
  left_join(ci_load_allcase) %>%
  mutate(Ratio = `Participated Load`/`Gross Total`) %>%
  mutate(`Tech Subsidy Cost` = `Tech Subsidy Cost`*Ratio) %>%
  select(case,year, `Tech Subsidy Cost`) 

# RPS ----
# Only for annual 100 and no cip
rpsycost_no24x7 <- read_csv(paste0(temp_RunFdr, "/CompiledResults/",subreg,
                                   "/Load/LSE_Payment_",subreg,"_with2019_and_DG.csv"),
                            col_types = cols()) %>%
  select(case,year,`Gross Total`,`RPS Total Payment`) %>%
  mutate(year = as.character(year)) %>%
  left_join(ci_load_allcase) %>%
  mutate(Ratio = `Participated Load`/`Gross Total`) %>%
  mutate(`RPS Total Payment` = `RPS Total Payment`*Ratio) %>%
  select(case,year, `RPS Total Payment`)%>%
  filter(grepl('nocip',case))

rpsycost_24x7 = tibble(
  case = character(),
  year = character(),
  `RPS Total Payment` = numeric()
)
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_rpscost_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Results/RPS_Cost.csv");

    temp_rps_cost <- read_csv(temp_rpscost_fn, col_types = cols())
    rps_cost = 0
    if (!grepl('nocip',cases[i])) {
      if (Studyregion == 'PJM') {
        rps_cost = sum(temp_rps_cost$RPS_LoadPayment_9+temp_rps_cost$RPS_LoadPayment_10)
      }
      if (Studyregion == 'WECC') {
        rps_cost = sum(temp_rps_cost$RPS_LoadPayment_4+temp_rps_cost$RPS_LoadPayment_5)
      }
      temp_row = as_tibble_row(c(case = cases[i], 
                                 year = years[j],
                                 `RPS Total Payment` = rps_cost))
      rpsycost_24x7 = rbind(rpsycost_24x7, temp_row)
    }
  }
}
rpsycost = rbind(rpsycost_no24x7, rpsycost_24x7)
rpsycost$`RPS Total Payment` = as.numeric(rpsycost$`RPS Total Payment`)
# rpsycost$`RPS Total Payment`[!grepl('nocip|annual100', rpsycost$case)] <-0 


# PPA of the C&I ----
# only works for 1 RPSH1 for now
ci_ppanet = tibble(
  case = character(),
  year = character(),
  `PPA Cost` = numeric(),
  `Gen Benefit` = numeric()
)
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_genprofit_fn <- paste0(temp_RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Results/NetRevenue.csv");
    temp_generator <- read_csv(temp_generator_fn, col_types = cols())
    ppa_rows = numeric()
    ppa_rows <- which((temp_generator$DR == 0) & (temp_generator$RPSH_1>0))
    if (grepl('annual100',cases[i])){
      ppa_rows <- which((temp_generator$DR == 0) & (temp_generator$RPSH_1>0) & (temp_generator$STOR == 0))
    }
    temp_genprofit = read_csv(temp_genprofit_fn,  col_types = cols()) %>%
      mutate(`PPA Cost` = (Fixed_OM_cost_MW + Fixed_OM_cost_MWh + Var_OM_cost_out +
                            Var_OM_cost_in + Fuel_cost + Charge_cost + EmissionsCost +
                            StartCost + Inv_cost_MW + Inv_cost_MWh + EmissionsCapture),
             `Gen Benefit` = (-1)*(EnergyRevenue + SubsidyRevenue + 
                                     ReserveMarginRevenue + RPSRevenue + RegSubsidyRevenue)) %>%
      select(`PPA Cost`, `Gen Benefit`)
    temp_genprofit = temp_genprofit[ppa_rows,]
    temp_row = as_tibble_row(c(case = cases[i],year = years[j], 
                               `PPA Cost` = sum(temp_genprofit$`PPA Cost`),
                               `Gen Benefit` = sum(temp_genprofit$`Gen Benefit`)))
    ci_ppanet = rbind(ci_ppanet, temp_row)
  }
}
# ci_ppanet$`PPA Cost`[grepl('nocip|annual100', ci_ppanet$case)] <-0
ci_ppanet$`PPA Cost`[grepl('nocip', ci_ppanet$case)] <-0
ci_ppanet$`PPA Cost` = as.numeric(ci_ppanet$`PPA Cost`)
# ci_ppanet$`Gen Benefit`[grepl('nocip|annual100', ci_ppanet$case)] <-0
ci_ppanet$`Gen Benefit`[grepl('nocip', ci_ppanet$case)] <-0
ci_ppanet$`Gen Benefit` = as.numeric(ci_ppanet$`Gen Benefit`)

# Combine all
ci_lsecost = left_join(ci_loadcost, select(ci_capacitycost,-`Participated Load`)) %>%
  left_join(trans_allocatedcost) %>%
  left_join(emissionrevenue) %>%
  left_join(techsubsidycost) %>%
  left_join(rpsycost) %>%
  left_join(ci_ppanet) %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))  %>%
  pivot_longer(cols = -c(case,year, Scenario, TechSensitivity, `Participated Load`)) %>%
  mutate(value_permwh = value/`Participated Load`)

write_csv(ci_lsecost, paste0(temp_RunFdr,'/CompiledResults/ci_lse_cost.csv'))






