# Calculate imputed Cost savings of DG
# Original DG time series
dg_mw <- read_csv('./data/Total_load_by_region_DG_subtracted.csv') %>%
  mutate(DG_MW = DG_MW - DG_MW_curtail) %>%
  select(SCENARIO,Year, LocalHourID, GenX.Region, DG_MW) %>%
  rename(SCENARIO_Load = SCENARIO) %>%
  mutate(Period = ceiling(LocalHourID/168))
dg_mw_capacity_factor = dg_mw %>%
  group_by(SCENARIO_Load,Year,GenX.Region) %>%
  summarize(capacity = max(DG_MW),
            capacityfactor= sum(DG_MW)/max(DG_MW)/8760) %>%
  ungroup()

for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_prices_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                             years[j],"_",cases[i],"/Results/prices_w.csv");
    temp_timeweight_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Results/time_weights.csv");
    temp_representativepoint_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                       years[j],"_",cases[i],"/Inputs/representative_point.csv");
    temp_LSERPSPayment_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                    years[j],"_",cases[i],"/Results/RPS_Cost.csv");
    if (file.exists(temp_prices_fn)){
      # temp_prices = as.data.frame(read.csv(temp_prices_fn));
      time_weight = read_csv(temp_timeweight_fn, col_types = cols())
      temp_prices <- read_csv(temp_prices_fn, col_types = cols())
      temp_representativepoint <- read_csv(temp_representativepoint_fn, col_types = cols()) %>%
        mutate(slot = as.numeric(str_remove(slot,'p')))
      n_period = length(temp_representativepoint$slot)
      n_hour = length(time_weight$Weight)
      n_hour_per_period = n_hour/n_period
      
      colnames(temp_prices) <- c("hour",zone_mapping$region);
      temp_prices$case = cases[i]
      temp_prices$year = years[j]
      # time_weight = read.csv(temp_timeweight_fn)
      
      temp_prices$weight = time_weight$Weight
      temp_prices$LocalHourID = rep((temp_representativepoint$slot-1)*n_hour_per_period,each = n_hour_per_period) + rep(c(1:n_hour_per_period),length = n_hour)
      temp_prices <- temp_prices %>%
        pivot_longer(zone_mapping$region);
      temp_dg_mw <- dg_mw %>% 
        filter(SCENARIO_Load == load_mapping$SCENARIO_Load[i],
               Year == years[j]) %>%
        select(-c(SCENARIO_Load, Year)) %>%
        rename(name = GenX.Region)
      temp_dg_energyrevenue = left_join(temp_prices,temp_dg_mw) %>%
        group_by(case,year,name) %>%
        summarize(energy_revenue = sum(DG_MW*value*weight),
                  energy_revenue_permwh = sum(DG_MW*value*weight)/sum(DG_MW*weight))
      if(!exists('dg_energyrevenue')) {
        dg_energyrevenue <- temp_dg_energyrevenue
      } else {
        dg_energyrevenue <- rbind(dg_energyrevenue, temp_dg_energyrevenue)
      }
      
      
      temp_Res_Mar_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],
                                "_",years[j],"_",cases[i],"/Results/ReserveMargin_w.csv");
      temp_Res_Mar = read_csv(temp_Res_Mar_fn, col_types = cols()) %>%
        rename(hour = Constraint) %>%
        mutate(weight = time_weight$Weight,
               LocalHourID = rep((temp_representativepoint$slot-1)*n_hour_per_period,each = n_hour_per_period) + rep(c(1:n_hour_per_period), length = n_hour),
               case = cases[i],
               year = years[j]) %>%
        pivot_longer(cols=!c('hour','LocalHourID','weight','case','year')) %>%
        mutate(value = round(value, digits = 3))
      n_capres = length(unique(temp_Res_Mar$name))
      region_capres_membership = as_tibble(cbind(
        Cap_Res_Mar = c('CapRes_1','CapRes_1','CapRes_1','CapRes_1','CapRes_1','CapRes_1','CapRes_1','CapRes_1','CapRes_1','CapRes_2','CapRes_2','CapRes_2','CapRes_2'),
        Region = c("PJM_COMD","PJM_Delaware","PJM_Dom","PJM_NJCoast","PJM_NJLand","PJM_PECO", "PJM_SMAC","PJM_WEST","PJM_WestMAC", "PJM_NJCoast","PJM_NJLand","PJM_PECO","PJM_Delaware")
      ))
      for (k in 1:n_capres) {
        temp_regions_per_constraint = region_capres_membership$Region[which(region_capres_membership$Cap_Res_Mar == paste0('CapRes_',k))]
        temp_Res_Mar_per_constraint = temp_Res_Mar %>%
          filter(name == paste0('CapRes_',k)) %>%
          select(-name)
        temp_dg_mw_per_constraint = filter(temp_dg_mw, name %in% temp_regions_per_constraint)
        temp_dg_capacityrevenue_per_constraint <- left_join(temp_Res_Mar_per_constraint, temp_dg_mw_per_constraint) %>%
          group_by(case,year,name) %>%
          summarize(capacity_revenue = sum(DG_MW*value*weight),
                    capacity_revenue_perMW = sum(DG_MW*value*weight)/max(DG_MW)) # note that because DG is directly subtracted from load, there is no derating factor for them
        if(!exists('temp_dg_capacityrevenue')) {
          temp_dg_capacityrevenue = temp_dg_capacityrevenue_per_constraint
        } else {
          temp_dg_capacityrevenue = rbind(temp_dg_capacityrevenue, temp_dg_capacityrevenue_per_constraint) %>%
            group_by(case,year,name) %>%
            summarize(capacity_revenue = sum(capacity_revenue),
                      capacity_revenue_perMW = sum(capacity_revenue_perMW)) # the second sum makes sense only because the capacity does not change (common denonminator)
        }
      }
      
      if(!exists('dg_capacityrevenue')) {
        dg_capacityrevenue <- temp_dg_capacityrevenue
      } else {
        dg_capacityrevenue <- rbind(dg_capacityrevenue, temp_dg_capacityrevenue)
      }
      rm(temp_dg_capacityrevenue, temp_dg_capacityrevenue_per_constraint)
      
      # This is the RPS revenue that is used to offset the RPS payment of LSE; because it is 
      temp_LSERPSPayment = read_csv(temp_LSERPSPayment_fn, 
                                    col_types = cols())[,-c(2,3)] # Columns 2 and 3 are RPS elgibile load and storage loss
      end = dim(temp_LSERPSPayment)[2]
      temp_dg_rpsrevenue = pivot_longer(temp_LSERPSPayment[-end], 
                                        c(2:(end-1)),
                                        names_to = "item") %>%
        mutate(case = cases[i], year = years[j]) %>%
        filter(grepl('RPS_DG_RevenueOffset_',item) == TRUE) %>%
        rename(zone = Zone) %>%
        mutate(zone = factor(zone)) %>%
        left_join(zone_mapping) %>%
        group_by(region, case, year) %>%
        summarise(rps_revenue = -1*round(sum(value),0)) %>%
        rename(name = region)
      temp_dg_capacity <- dg_mw_capacity_factor %>%
        filter(SCENARIO_Load == load_mapping$SCENARIO_Load[i],
               Year == years[j]) %>%
        select(-c(SCENARIO_Load, Year)) %>%
        rename(name = GenX.Region)
      temp_dg_rpsrevenue <- left_join(temp_dg_rpsrevenue, temp_dg_capacity) %>%
        mutate(rps_revenue_per_mwh = rps_revenue/capacity/capacityfactor/8760)
      if(!exists('dg_rpsrevenue')) {
        dg_rpsrevenue <- temp_dg_rpsrevenue
      } else {
        dg_rpsrevenue <- rbind(dg_rpsrevenue, temp_dg_rpsrevenue)
      }
    }
  }
}

dg_energyrevenue <- dg_energyrevenue %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
dg_capacityrevenue <- dg_capacityrevenue %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
dg_rpsrevenue <- dg_rpsrevenue %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))

write_csv(dg_energyrevenue, './data/dg_energyrevenue.csv')
write_csv(dg_capacityrevenue, './data/dg_capacityrevenue.csv')
write_csv(dg_rpsrevenue, './data/dg_rpsrevenue.csv')



nonsrec_dgcapacity = dg_mw_capacity_factor %>%
  filter(grepl('PJM_NJ', GenX.Region),
         Year %in% c(2030, 2040, 2050)) 
temp_rows = which(nonsrec_dgcapacity$GenX.Region == 'PJM_NJCoast')
nonsrec_dgcapacity$capacity[temp_rows] = nonsrec_dgcapacity$capacity[temp_rows] - 4503/1.2*0.342*(1+0.0453)
temp_rows = which(nonsrec_dgcapacity$GenX.Region == 'PJM_NJLand')
nonsrec_dgcapacity$capacity[temp_rows] = nonsrec_dgcapacity$capacity[temp_rows] - 4503/1.2*0.658*(1+0.0453) #4503 is the total capacity installed + pipeline + remaining dg solar
nonsrec_dgcapacity <- nonsrec_dgcapacity %>% 
  mutate(output = capacity* capacityfactor*8760) %>%
  rename(year = Year, name = GenX.Region) %>%
  mutate(year = as.character(year))

nonsrec_dg_energyrevenue <- dg_energyrevenue %>%
  filter(grepl('PJM_NJ', name)) %>%
  left_join(load_mapping, by = c('case' = 'case_description')) %>%
  left_join(nonsrec_dgcapacity) %>%
  group_by(case, year, Scenario, TechSensitivity) %>%
  summarize(nonsrec_energy_revenue = sum(energy_revenue_permwh*output)) 
write_csv(nonsrec_dg_energyrevenue, './data/nonsrec_dg_energyrevenue.csv')


nonsrec_dg_capacityrevenue <- dg_capacityrevenue %>%
  filter(grepl('PJM_NJ', name)) %>%
  left_join(load_mapping, by = c('case' = 'case_description')) %>%
  left_join(nonsrec_dgcapacity) %>%
  group_by(case, year, Scenario, TechSensitivity) %>%
  summarize(nonsrec_capacity_revenue = sum(capacity_revenue_perMW*capacity))
write_csv(nonsrec_dg_capacityrevenue, './data/nonsrec_dg_capacityrevenue.csv')



nonsrec_dg_rpsrevenue <- dg_rpsrevenue %>%
  filter(grepl('PJM_NJ', name)) %>%
  left_join(load_mapping, by = c('case' = 'case_description')) %>%
  select(-c(capacity,capacityfactor)) %>%
  left_join(nonsrec_dgcapacity) %>%
  group_by(case, year, Scenario, TechSensitivity) %>%
  summarize(nonsrec_rps_revenue = sum(round(rps_revenue_per_mwh*output,0))) 
write_csv(nonsrec_dg_rpsrevenue, './data/nonsrec_dg_rpsrevenue.csv')

nonsrec_dgcapacity_total <- nonsrec_dgcapacity %>%
  group_by(SCENARIO_Load, year) %>%
  summarize(capacity = sum(capacity), output = sum(output))
nonsrec_revenue_total <- left_join(nonsrec_dg_energyrevenue, nonsrec_dg_capacityrevenue) %>%
  left_join(nonsrec_dg_rpsrevenue) %>%
  mutate(nonsrec_revenue = nonsrec_energy_revenue + nonsrec_rps_revenue + nonsrec_capacity_revenue) %>%
  left_join(load_mapping, by = c('case' = 'case_description')) %>%
  left_join(nonsrec_dgcapacity_total) %>%
  select(-SCENARIO_Load) %>%
  mutate(capacityfactor = output/capacity/8760,
         nonsrec_revenue_permwh = nonsrec_revenue/output)
write_csv(nonsrec_revenue_total, './data/nonsrec_dg_revenue.csv')

dg_energyrevenue_nj <- dg_energyrevenue %>%
  filter(grepl('PJM_NJ',name)) %>%
  group_by(case, year, Scenario, TechSensitivity) %>%
  summarize(energy_revenue = sum(energy_revenue))
write_csv(dg_energyrevenue_nj, './data/dg_energyrevenue_nj.csv')
dg_capacityrevenue_nj <- dg_capacityrevenue %>%
  filter(grepl('PJM_NJ',name)) %>%
  group_by(case, year, Scenario, TechSensitivity) %>%
  summarize(capacity_revenue = sum(capacity_revenue))
write_csv(dg_capacityrevenue_nj, './data/dg_capacityrevenue_nj.csv')
dg_rpsrevenue_nj <- dg_rpsrevenue %>%
  filter(grepl('PJM_NJ',name)) %>%
  group_by(case, year, Scenario, TechSensitivity) %>%
  summarize(rps_revenue = sum(rps_revenue))
write_csv(dg_rpsrevenue_nj, './data/dg_rpsrevenue_nj.csv')




rm(dg_energyrevenue, temp_dg_energyrevenue, 
   dg_capacityrevenue, temp_dg_capacityrevenue,
   dg_rpsrevenue, temp_dg_rpsrevenue)
