#LSE Payment cost Plot
# source('./code/Header.R')
# Calculate subregion cost
lse_payment <- read_csv(paste0(RunFdr,'/CompiledResults/LSE_Payment.csv'))
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
  if (file.exists(lse_payment_subregion_fn)){
    lse_payment_plot <- read_csv(lse_payment_subregion_fn) %>%
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
    if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
      for (j in 1:n_case_temp) {
        lse_payment_dg <- read_csv('./data/LSE_DGCost_NJ.csv')
        if (grepl('dgsolar',case_temp$case[j])){
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
    }
    lse_payment_wide <- pivot_wider(lse_payment_plot, names_from = `Cost Type`);
    lse_payment_wide[is.na(lse_payment_wide)] <- 0;
    if (!("NJ DG Cost" %in% colnames(lse_payment_wide))){
      lse_payment_wide$`NJ DG Cost` = 0
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



