# Generation Output
# # 
# source('./code/Header.R')
gen_power <- read_csv(paste0(RunFdr,'CompiledResults/power.csv'), 
                      col_types = cols()) %>%
  left_join(resource_mapping) %>%
  filter(!(Fuel %in% storage_fuel)) %>%
  filter(!(Fuel %in% flexiload_list)) %>%
  na.omit()
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  genoutput_subregion <- gen_power%>%
    filter(Region %in% temp_total) %>%
    group_by(case, year, Fuel) %>%
    summarize(AnnualOutput = sum(AnnualSum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualOutput)
  write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                       '/Generation/Gen_Output_',temp_total_title,".csv"))
  # dg_output_fn <- paste0(RunFdr,'/Total_load_by_region_DG_subtracted_summary.csv')
  #   if (file.exists(dg_output_fn)) {
  #     if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
  #     genoutput_runs_dg <- read_csv(dg_output_fn, col_types = cols()) %>%
  #       # pivot_wider(id_cols = c(SCENARIO,Year,GenX.Region), names_from = 'Load_Type',values_from = 'TWh') %>%
  #       filter(Year %in% years) %>%
  #       rename(region = `GenX.Region`,year = Year) %>%
  #       rename(`SCENARIO_Load` = `SCENARIO`) %>%
  #       mutate(Fuel = 'DG Solar') %>%
  #       filter(region %in% temp_total) %>%
  #       group_by(year,Fuel,`SCENARIO_Load`) %>%
  #       summarize(AnnualOutput = 1e6*sum(dg_TWh - dg_TWh_curtail)) %>%
  #       select(SCENARIO_Load,year,Fuel, AnnualOutput) %>%
  #       right_join(load_mapping) %>%
  #       rename(`case` = `case_description`) %>%
  #       left_join(cases_newnames, by = c('case' = 'case_description')) %>%
  #       select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualOutput)
  #     genoutput_subregion <- rbind(genoutput_subregion, genoutput_runs_dg)
  #     write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
  #                                          '/Generation/Gen_Output_',temp_total_title,"_withDG.csv"))
  #     } else {
  #       genoutput_runs_dg <- read_csv(dg_output_fn, col_types = cols()) %>%
  #         pivot_wider(id_cols = c(SCENARIO,Year,GenX.Region),names_from = 'Load_Type',values_from = 'TWh') %>%
  #         filter(Year %in% years) %>%
  #         rename(region = `GenX.Region`,year = Year) %>%
  #         rename(`SCENARIO_Load` = `SCENARIO`) %>%
  #         mutate(Fuel = 'DG Solar') %>%
  #         filter(region %in% temp_total) %>%
  #         group_by(year,Fuel,`SCENARIO_Load`) %>%
  #         summarize(AnnualOutput = -1*1e6*sum(DG_TWh - DG_curtail_TWh)) %>%
  #         select(SCENARIO_Load,year,Fuel, AnnualOutput) %>%
  #         right_join(load_mapping) %>%
  #         rename(`case` = `case_description`) %>%
  #         left_join(cases_newnames, by = c('case' = 'case_description')) %>%
  #         select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualOutput)
  #       genoutput_subregion <- rbind(genoutput_subregion, genoutput_runs_dg)
  #       write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
  #                                            '/Generation/Gen_Output_',temp_total_title,"_withDG.csv"))        
  #     }
  #   } else {
  #     print('there is no DG generation file')
  #   }
}

