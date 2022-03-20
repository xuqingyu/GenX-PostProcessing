if (exists('load_payment')) {rm(load_payment, load_payment_withname)}
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_timeweight_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Results/time_weights.csv")
    temp_timeweight <- read_csv(temp_timeweight_fn, col_types = cols())
    temp_energyprice_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                  years[j],"_",cases[i],"/Results/prices.csv");
    temp_energyprice <- read_csv(temp_energyprice_fn, col_types = cols())[,-1]
    temp_capres_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Inputs/Capacity_reserve_margin.csv");
    temp_capres <- read_csv(temp_capres_fn, col_types = cols())[,-c(1,2)] %>% 
      as.matrix() 
    temp_capacityprice_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                    years[j],"_",cases[i],"/Results/ReserveMargin.csv") %>%
      as.matrix()
    temp_capacityprice <- read_csv(temp_capacityprice_fn, col_types = cols())[,-1]
    representative_point <- read_csv(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                            years[j],"_",cases[i],'/Inputs/Representative_Period.csv'),
                                     col_types = cols())
    reduced_load_file <- read_csv(paste0(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                                years[j],"_",cases[i], '/Inputs/Load_data.csv')), col_types = cols())
    reduced_load = reduced_load_file[,-c(1:9)]
    n_period = reduced_load_file$Rep_Periods[1]
    n_hour_per_period = reduced_load_file$Timesteps_per_Rep_Period[1]
    n_totalhour = n_hour_per_period*n_period
    original_hourid = ((rep(as.numeric(str_remove(representative_point$slot,'p')), 
                            each = n_hour_per_period) - 1) * n_hour_per_period + 
                         rep(c(1:n_hour_per_period), times = n_period))
    for (k in 1:n_tfs) {
      cfe_load_ori <- read_csv(paste0(RunFdr,Studyregion,"_RPSH_Load_data_", n_tfs,".csv"), col_types = cols())[,-1]
      temp_cfeload_all = cfe_load_ori[original_hourid, ]
      if (cistudy == 1) {
        if (grepl('_5ci', cases[i])) {
          CIPurchaseRatio = 0.05
        }
        if (grepl('_10ci', cases[i])) {
          CIPurchaseRatio = 0.10
        }
        if (grepl('_25ci', cases[i])) {
          CIPurchaseRatio = 0.25
        }
        temp_cfeload_all = round(temp_cfeload_all*CIPurchaseRatio,0)
      } else {
        temp_cfeload_all = round(temp_cfeload_all,0)
      }
      temp_energypayment = sum(t(temp_cfeload_all * temp_energyprice) %*% temp_timeweight$Weight)

      temp_capres_requirement = (1+apply(temp_capres,2,countercapres)) %>% as.matrix()
      
      temp_capacitypayment = sum(t((as.matrix(temp_cfeload_all) %*% temp_capres_requirement) * temp_capacityprice) %*% temp_timeweight$Weight)
      
      participatedload = sum(t(temp_cfeload_all) %*% temp_timeweight$Weight)
      temp_row = as_tibble_row(c(case = cases[i], 
                                 year = years[j],
                                 Policy = k,
                                 `Energy Payment` = as.numeric(temp_energypayment),
                                 `Capacity Payment` = as.numeric(temp_capacitypayment),
                                 `Participated Load` = as.numeric(participatedload)))
      if (!exists('load_payment')) {
        load_payment <- temp_row
        rm(temp_row, temp_energypayment, temp_capacitypayment, participatedload, temp_cfeload_all,CIPurchaseRatio, cfe_load_ori)
      } else {
        load_payment <- rbind(load_payment, temp_row)
        rm(temp_row, temp_energypayment, temp_capacitypayment, participatedload, temp_cfeload_all,CIPurchaseRatio, cfe_load_ori)
      }
    }
  }
}
load_payment_withname <- left_join(load_payment, cases_newnames, by = c('case' = 'case_description')) %>%
  select(case,year,Scenario, TechSensitivity, Policy, `Energy Payment`,`Capacity Payment`, `Participated Load`)

  # mutate(`Energy Payment ($/MWh)` = as.numeric(`Energy Payment`)/as.numeric(`Participated Load`))
write_csv(load_payment, paste0(RunFdr,"/CompiledResults/tfs_load_cost.csv"))
rm(temp_timeweight_fn,temp_timeweight,representative_point,reduced_load_file)
rm(temp_energyprice_fn,temp_energyprice)
rm(temp_capres_fn,temp_capres,temp_capacityprice_fn,temp_capacityprice)
rm(reduced_load, n_period,n_hour_per_period,n_totalhour,original_hourid)
# rm(load_payment,load_payment_withname)

# Transmission cost ----
# use the allocated transmission cost 
# For CA, no existing transmission is added.
load_allcase = select(load_payment_withname, c(case,year,Scenario, TechSensitivity,Policy, `Participated Load`))
dg = read_csv(paste0(RunFdr, "/CompiledResults/",subreg,
                     "/Generation/Gen_Output_",subreg,".csv"),
              col_types = cols()) %>%
  filter(Fuel == 'DG Solar') %>%
  select(case,year,AnnualOutput) %>%
  rename(DG_AnnualOutput = AnnualOutput)
# note that in the 2022 runs of 24/7 DG is a separate resources so gross load is the calcualted annual laod;
# transmission level load is gross load - annual load.
load_allocatedcost <- read_csv(paste0(RunFdr, "/CompiledResults/",subreg,
                                       "/Load/LSE_Payment_",subreg,".csv"),
                                col_types = cols()) %>%
  select(case, year, AnnualLoad,`Transmission Cost`,`Congestion Revenue`,
         `CO2 Revenue`,`Tech Subsidy Cost`,`RPS Total Payment`) %>%
  left_join(dg) %>%
  mutate(year = as.character(year), 
         `Gross Total` = as.numeric(AnnualLoad)) %>%  
  mutate(TransLoad = `Gross Total` - DG_AnnualOutput) %>%
  left_join(load_allcase) %>%
  mutate(`Participated Load`  = as.numeric(`Participated Load`)) %>%
  mutate(TransRatio = `Participated Load`/TransLoad,
         GrossRatio = `Participated Load`/`Gross Total`) %>%
  mutate(`Transmission Cost` = `Transmission Cost`*TransRatio,
         `Congestion Revenue` = `Congestion Revenue`*TransRatio,
         `CO2 Revenue` = `CO2 Revenue`*GrossRatio,
         `Tech Subsidy Cost` = `Tech Subsidy Cost`*GrossRatio,
         `RPS Total Payment` = `RPS Total Payment`*GrossRatio) %>%
  select(case, year, Scenario, TechSensitivity, Policy, `Transmission Cost`,
         `Congestion Revenue`,`CO2 Revenue`,`Tech Subsidy Cost`,
         `RPS Total Payment`, `Participated Load`)
load_allocatedcost$`RPS Total Payment`[which(!grepl('nocip',load_allocatedcost$case))] <- 0 # RPS total payment only make sense for no cip cases

# for all other cases which separate RPS constraints
if (exists('ppanet')) {rm(ppanet)}
if (exists('drsavings')) {rm(drsavings)}
for ( i in 1:length(cases)) {
  for (j in 1:length(years)) {
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    temp_genprofit_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Results/NetRevenue.csv");
    temp_genprofit_all <- read_csv(temp_genprofit_fn, col_types = cols())
    temp_generator <- read_csv(temp_generator_fn, col_types = cols())
    for (k in 1:n_tfs) {
      if (!grepl('nocip', cases[i])) {
        policy_column = which(colnames(temp_generator) ==paste0('RPSH_',k))
        cfe_rows = which(temp_generator[,policy_column] == 1)
        dr_rows = which(temp_generator$FLEX == 1)
        temp_genprofit <- temp_genprofit_all[setdiff(cfe_rows, dr_rows), ] %>%
          mutate(Policy = k, case = cases[i], year = years[j]) %>%
          mutate(`PPA Cost` = (Fixed_OM_cost_MW + Fixed_OM_cost_MWh + Var_OM_cost_out +
                                 Var_OM_cost_in + Fuel_cost + Charge_cost + EmissionsCost +
                                 StartCost + Inv_cost_MW + Inv_cost_MWh), # + EmissionsCapture
                 `Gen Benefit` = (-1)*(EnergyRevenue + SubsidyRevenue + 
                                         ReserveMarginRevenue + ESRRevenue + RegSubsidyRevenue)) %>%
          select(case, year, Policy, `PPA Cost`, `Gen Benefit`, ESRRevenue) %>%
          group_by(case, year, Policy) %>%
          summarise(`PPA Cost` = sum(`PPA Cost`),
                    `Gen Benefit` = sum(`Gen Benefit`),
                    ESRRevenue = sum(ESRRevenue)) %>%
          mutate(Policy = as.character(Policy))
        temp_drsaving <- temp_genprofit_all[intersect(cfe_rows, dr_rows), ] %>%
          mutate(Policy = k, case = cases[i], year = years[j]) %>%
          mutate(`Energy Savings` = (Charge_cost - EnergyRevenue),
                 `Capacity Savings` = (-1)* (ReserveMarginRevenue)) %>%
          select(case, year, Policy,  `Energy Savings`,`Capacity Savings`) %>%
          group_by(case, year, Policy) %>%
          summarise(`Energy Savings` = sum(`Energy Savings`),
                    `Capacity Savings` = sum(`Capacity Savings`)) %>%
          mutate(Policy = as.character(Policy))
      } else {
        temp_genprofit <- as_tibble(cbind(case = cases[i], year = years[j],Policy = k,
                                              `PPA Cost`= 0,`Gen Benefit` = 0, ESRRevenue = 0)) %>%
          mutate(`PPA Cost` = as.numeric(`PPA Cost`),
                 `Gen Benefit` = as.numeric(`Gen Benefit`),
                 ESRRevenue = as.numeric(ESRRevenue))
        temp_drsaving <- as_tibble(cbind(Policy = k, case = cases[i], year = years[j],
                                             `Energy Savings`= 0,`Capacity Savings` = 0)) %>%
          mutate(`Energy Savings` = as.numeric(`Energy Savings`),
                 `Capacity Savings` = as.numeric(`Capacity Savings`))
      }
      if(!exists('ppanet')) {
        ppanet <- temp_genprofit;
        rm(temp_genprofit, cfe_rows,policy_column)
      } else {
        ppanet <- rbind(ppanet, temp_genprofit)
        rm(temp_genprofit, cfe_rows,policy_column)
      }
      if(!exists('drsavings')) {
        drsavings <- temp_drsaving;
        rm(temp_drsaving)
      } else {
        drsavings <- rbind(drsavings, temp_drsaving)
        rm(temp_drsaving)
      }
    }
  }
}
rm(temp_generator_fn, temp_genprofit_fn,temp_genprofit_all, temp_generator )
load_payment_withname_combined <- left_join(load_payment_withname, drsavings) %>%
  mutate(`Energy Payment` = as.numeric(`Energy Payment`),
         `Capacity Payment` = as.numeric(`Capacity Payment`)) %>%
  mutate(`Energy Payment` = `Energy Payment` + `Energy Savings`,
         `Capacity Payment` = `Capacity Payment` + `Capacity Savings`) %>%
  mutate(`Participated Load` = as.numeric(`Participated Load`)) %>%
  select(-c(`Energy Savings`, `Capacity Savings`)) %>%
  left_join(load_allocatedcost) %>%
  left_join(ppanet);
nontfsrows = which(!grepl('nocip',load_payment_withname_combined$case))
load_payment_withname_combined$`RPS Total Payment`[nontfsrows] <- load_payment_withname_combined$ESRRevenue[nontfsrows]
load_payment_withname_combined <- select(load_payment_withname_combined, -c(ESRRevenue))

load_payment_withname_combined_long <- load_payment_withname_combined %>%
  pivot_longer(cols = -c(case,year, Scenario, TechSensitivity, Policy, `Participated Load`)) %>%
  mutate(value_permwh = value/`Participated Load`)

write_csv(load_payment_withname_combined_long, paste0(RunFdr,'/CompiledResults/tfs_lse_cost.csv'))
rm(load_payment_withname_combined_long, load_payment_withname_combined,nontfsrows)

