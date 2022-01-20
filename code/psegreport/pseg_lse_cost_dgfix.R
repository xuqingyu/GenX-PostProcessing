#LSE Payment cost Plot
# source('./code/Header.R')
# Calculate subregion cost
# Modified on Sep. 01, 2021
# Modified on Jan. 11, 2022: takeout imputed revenues from DG from LSE cost and lower down the capital cost of DG
# works in general
lse_payment <- read_csv(paste0(RunFdr,'/CompiledResults/LSE_Payment.csv'), col_types = cols())
for ( i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_payment_subregion <- lse_payment %>%
    filter(region %in% temp_total) %>%
    group_by(case,year) %>%
    summarize(AnnualLoad = sum(AnnualLoad),
              `Energy Payment` = sum(`Energy Payment`),
              `Transmission Loss Cost` = sum(`Transmission Loss Cost`),
              `NSE Cost` = sum(`NSE Cost`),
              `Capacity Payment` = sum(`Capacity Payment`),
              `CO2 Revenue` = sum(`CO2 Revenue Mass Cap` + `CO2 Revenue Load Rate Cap` + `CO2 Revenue Tax`),
              `RPS Total Payment` = sum(`RPS Total Payment`),
              `Tech Subsidy Cost` = sum(`Tech Subsidy Cost`),
              `Transmission Cost` = sum(`Transmission Cost`),
              `Congestion Revenue` = sum(`Congestion Revenue`)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case, year, Scenario, `TechSensitivity`,AnnualLoad,
           `Energy Payment`, `Transmission Loss Cost`, `NSE Cost`, `Capacity Payment`,
           `CO2 Revenue`,`RPS Total Payment`,`Tech Subsidy Cost`,
           `Transmission Cost`, `Congestion Revenue`)
  write_csv(lse_payment_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_',temp_total_title,".csv"))
}
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  lse_payment_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_',temp_total_title,".csv")
  if (file.exists(lse_payment_subregion_fn)) {
    lse_payment_plot <- read_csv(lse_payment_subregion_fn, col_types = cols()) %>%
      pivot_longer(!c(case, year, Scenario, `TechSensitivity`,AnnualLoad ),names_to = 'Cost Type');
    case_temp <- unique(select(lse_payment_plot,case))
    n_case_temp <- dim(case_temp)[1]
    if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
      if (temp_total_title == 'New Jersey'){
        lse_payment_2019_base <- read_csv('./data/LSE_Payment_2019_NJ.csv')
      } else if (temp_total_title == 'PJM'){
        lse_payment_2019_base <- read_csv('./data/LSE_Payment_2019_PJM.csv')
      }
      for (j in 1:n_case_temp) {
        temp_lse_payment_2019 <- lse_payment_2019_base;
        temp_lse_payment_2019$case <- case_temp$case[j]
        temp_lse_payment_2019 <- left_join(temp_lse_payment_2019, cases_newnames, by = c('case' = 'case_description')) %>%
          select(case, year, Scenario, `TechSensitivity`,AnnualLoad, `Cost Type`, value)
        lse_payment_plot <- rbind(lse_payment_plot,temp_lse_payment_2019)
      }
    }
    # Adding DG capital cost Cost
    if ( ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) & identical(years,c("2030","2040","2050"))) {
      for (j in 1:n_case_temp) {
        if (grepl('dgsolar',case_temp$case[j])){
          lse_payment_dg <- read_csv('./data/LSE_DGCost_NJ_HighDG.csv', col_types = cols())
        } else {
          lse_payment_dg <- read_csv('./data/LSE_DGCost_NJ_Normal.csv', col_types = cols())
        }
        temp_load <- select(lse_payment_plot,case,year,AnnualLoad) %>%
          filter(case == case_temp$case[j])
        if (grepl('lowrecost',case_temp$case[j])){
          lse_payment_dg <- lse_payment_dg %>% 
            filter(`TechSensitivity` == 'Low RE Cost')
        } else if (grepl('highrecost',case_temp$case[j])){
          lse_payment_dg <- lse_payment_dg %>% 
            filter(`TechSensitivity` == 'High RE Cost')
        } else {
          lse_payment_dg <- lse_payment_dg %>% 
            filter(`TechSensitivity` == 'Medium RE Cost')
        }
        lse_payment_dg <- select(lse_payment_dg, -c(`TechSensitivity`))
        temp_lse_payment_dg <- unique(left_join(temp_load,lse_payment_dg));
        temp_lse_payment_dg <- left_join(temp_lse_payment_dg, cases_newnames, by = c('case' = 'case_description')) %>%
          select(case, year, Scenario, `TechSensitivity`,AnnualLoad, `Cost Type`, value)
        lse_payment_plot <- rbind(lse_payment_plot,temp_lse_payment_dg)
      }
    }
    

    lse_payment_wide <- pivot_wider(lse_payment_plot, names_from = `Cost Type`);
    lse_payment_wide[is.na(lse_payment_wide)] <- 0;
    if (!("NJ DG Cost" %in% colnames(lse_payment_wide))){
      lse_payment_wide$`NJ DG Cost` = 0
    }
    
    if ( ((temp_total_title == 'New Jersey')) & identical(years,c("2030","2040","2050"))) {
      energyrevenuefix <- read_csv('./data/dg_energyrevenue_nj.csv', col_types = cols())
      # capacityrevenuefix <- read_csv('./data/dg_capacityrevenue_nj.csv', col_types = cols())
      rpsrevenuefix <- read_csv('./data/dg_rpsrevenue_nj.csv', col_types = cols())
      lse_payment_wide <- left_join(lse_payment_wide, energyrevenuefix);
      lse_payment_wide$energy_revenue[which(is.na(lse_payment_wide$energy_revenue))] <-0
      lse_payment_wide <- lse_payment_wide %>%
        mutate(`Energy Payment` = `Energy Payment` + energy_revenue) %>% # giving all energy revenue back to dg
        select(-energy_revenue);
      
      # lse_payment_wide <- left_join(lse_payment_wide, capacityrevenuefix);
      # lse_payment_wide$capacity_revenue[which(is.na(lse_payment_wide$capacity_revenue))] <-0
      # lse_payment_wide <- lse_payment_wide %>%
      #   mutate(`Capacity Payment` = `Capacity Payment` + capacity_revenue) %>%
      #   select(-capacity_revenue);
      
      lse_payment_wide <- left_join(lse_payment_wide, rpsrevenuefix);
      lse_payment_wide$rps_revenue[which(is.na(lse_payment_wide$rps_revenue))] <-0
      lse_payment_wide <- lse_payment_wide %>%
        mutate(`RPS Total Payment` = `RPS Total Payment` + `rps_revenue`) %>% # giving all RPS revenue back to dg
        select(-rps_revenue)
      
      total_urec_revenue_fix <- read_csv('./data/nonsrec_dg_revenue.csv', col_types = cols()) %>%
        select(case, year , Scenario, TechSensitivity, nonsrec_energy_revenue, nonsrec_rps_revenue)
      
      lse_payment_wide <- left_join(lse_payment_wide, total_urec_revenue_fix);
      
      lse_payment_wide$nonsrec_energy_revenue[which(is.na(lse_payment_wide$nonsrec_energy_revenue))] <-0
      lse_payment_wide$nonsrec_rps_revenue[which(is.na(lse_payment_wide$nonsrec_rps_revenue))] <-0
      # because the incremental dg is covered by the capital cost, their energy revenue is given back to LSEs
      lse_payment_wide <- lse_payment_wide%>%
        mutate(`Energy Payment` = `Energy Payment` - nonsrec_energy_revenue, 
               `RPS Total Payment` = `RPS Total Payment` - nonsrec_rps_revenue) %>%
        select(-c(nonsrec_rps_revenue, nonsrec_energy_revenue))
    }
    
    
    gross_load_fn <- paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Load/Load_Component_",Subregions[i],".csv");
    if (file.exists(gross_load_fn)) {
      gross_load <- read_csv(gross_load_fn) %>%
        filter(`Load Type` == 'Gross Total') %>%
        mutate(`Gross Total` = TWh*1e6) %>%
        select(-c(`Load Type`,TWh))
      lse_payment_wide <- left_join(lse_payment_wide, gross_load);
      if (Subregions[i] == 'PJM'){
        lse_payment_wide$`Gross Total`[lse_payment_wide$year == 2019] <- 799629063
      } else if (Subregions[i] == 'New Jersey'){
        lse_payment_wide$`Gross Total`[lse_payment_wide$year == 2019] <- 82834833
      } else {
        lse_payment_wide$`Gross Total`[lse_payment_wide$year == 2019] <- lse_payment_wide$`AnnualLoad`[lse_payment_wide$year == 2019]
      }
      
      write_csv(lse_payment_wide,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_',temp_total_title,"_with2019_and_DG.csv"))
      lse_payment_long <- pivot_longer(lse_payment_wide,!c(case, year, Scenario, `TechSensitivity`,AnnualLoad,`Gross Total`),names_to = 'Cost Type') %>%
        mutate(`USD per MWh` = value/`Gross Total`);
      write_csv(lse_payment_long,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_long_',temp_total_title,"_with2019_and_DG.csv"))
    } else {
      print('Gross load file is missing, use AnnualLoad instead')
      lse_payment_wide$`Gross Total` <- lse_payment_wide$`AnnualLoad`
      write_csv(lse_payment_wide,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_',temp_total_title,"_with2019_and_DG.csv"))
      lse_payment_long <- pivot_longer(lse_payment_wide,!c(case, year, Scenario, `TechSensitivity`,AnnualLoad,`Gross Total`),names_to = 'Cost Type') %>%
        mutate(`USD per MWh` = value/`Gross Total`);
      write_csv(lse_payment_long,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Load/LSE_Payment_long_',temp_total_title,"_with2019_and_DG.csv"))
    }
  }
}



