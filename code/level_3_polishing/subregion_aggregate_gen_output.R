# Generation Output
# # 
# source('./code/Header.R')
gen_power <- read_csv(paste0(RunFdr,'CompiledResults/power.csv'), 
                      col_types = cols()) %>%
  left_join(resource_mapping) %>%
  filter(!(Fuel %in% storage_fuel)) %>%
  na.omit()
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  genoutput_subregion <- gen_power%>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel) %>%
    summarize(AnnualOutput = sum(AnnualSum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Fuel, Scenario, `TechSensitivity`,
           AnnualOutput)
  write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                       '/Generation/Gen_Output_',temp_total_title,".csv"))
  dg_output_fn <- paste0(RunFdr,'/Total_load_by_region_DG_subtracted_summary.csv')
    if (file.exists(dg_output_fn)) {
      if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
      genoutput_runs_dg <- read_csv(dg_output_fn, col_types = cols()) %>%
        # pivot_wider(id_cols = c(SCENARIO,Year,GenX.Region), names_from = 'Load_Type',values_from = 'TWh') %>%
        filter(Year %in% years) %>%
        rename(region = `GenX.Region`,year = Year) %>%
        rename(`SCENARIO_Load` = `SCENARIO`) %>%
        mutate(Fuel = 'DG Solar') %>%
        filter(region %in% temp_total) %>%
        group_by(year,Fuel,`SCENARIO_Load`) %>%
        summarize(AnnualOutput = 1e6*sum(dg_TWh - dg_TWh_curtail)) %>%
        select(SCENARIO_Load,year,Fuel, AnnualOutput) %>%
        right_join(load_mapping) %>%
        rename(`case` = `case_description`) %>%
        left_join(cases_newnames, by = c('case' = 'case_description')) %>%
        select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualOutput)
      genoutput_subregion <- rbind(genoutput_subregion, genoutput_runs_dg)
      write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                           '/Generation/Gen_Output_',temp_total_title,"_withDG.csv"))
      } else {
        genoutput_runs_dg <- read_csv(dg_output_fn, col_types = cols()) %>%
          pivot_wider(id_cols = c(SCENARIO,Year,GenX.Region),names_from = 'Load_Type',values_from = 'TWh') %>%
          filter(Year %in% years) %>%
          rename(region = `GenX.Region`,year = Year) %>%
          rename(`SCENARIO_Load` = `SCENARIO`) %>%
          mutate(Fuel = 'DG Solar') %>%
          filter(region %in% temp_total) %>%
          group_by(year,Fuel,`SCENARIO_Load`) %>%
          summarize(AnnualOutput = -1*1e6*sum(DG_TWh - DG_curtail_TWh)) %>%
          select(SCENARIO_Load,year,Fuel, AnnualOutput) %>%
          right_join(load_mapping) %>%
          rename(`case` = `case_description`) %>%
          left_join(cases_newnames, by = c('case' = 'case_description')) %>%
          select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualOutput)
        genoutput_subregion <- rbind(genoutput_subregion, genoutput_runs_dg)
        write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                             '/Generation/Gen_Output_',temp_total_title,"_withDG.csv"))        
      }
    } else {
      print('there is no DG generation file')
    }
}

# for (i in 1:n_subregions) {
#   temp_total_title <- Subregions[i]
#   temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
#   # for PJM study
#   if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
#     genoutput_subregion <- read_csv(paste0(RunFdr,'/CompiledResults/',Subregions[i],
#                                            '/Generation/Gen_Output_',temp_total_title,"_withDG.csv"))
#     gen_output_2019_base <- read_csv('./data/Gen_output_2019.csv') %>%
#       left_join(resource_mapping) %>%
#       group_by(year,zone,region,Fuel) %>%
#       summarise(AnnualOutput = sum(Sum))%>%
#       filter(!(Fuel %in% storage_fuel)) 
#     case_temp <- unique(select(genoutput_subregion,case))
#     n_case_temp <- dim(case_temp)[1]
#     for (j in 1:n_case_temp) {
#       temp_gen_output_2019_base <- gen_output_2019_base %>%
#         filter(region %in% temp_total) %>%
#         group_by(year,Fuel) %>%
#         summarise(AnnualOutput = sum(AnnualOutput))
#       temp_gen_output_2019_base$case <- case_temp$case[j]
#       temp_gen_output_2019_base <- left_join(temp_gen_output_2019_base, cases_newnames, 
#                                              by = c('case' = 'case_description')) %>%
#         select(case, year, Fuel, Scenario, `TechSensitivity`,AnnualOutput)
#       genoutput_subregion <- rbind(genoutput_subregion,temp_gen_output_2019_base)
#     }
#     genoutput_2019_dg <- read_csv('./data/PJM_DG_2019.csv') %>%
#       rename(region = `GenX.Region`) %>%
#       mutate(AnnualOutput = 1e6*`DG_TWh`) %>%
#       left_join(zone_mapping) %>%
#       mutate(Fuel = 'DG Solar',year = 2019) %>%
#       select(year,zone,region,Fuel,AnnualOutput)
#     case_temp <- unique(select(genoutput_subregion,case))
#     n_case_temp <- dim(case_temp)[1]
#     for (j in 1:n_case_temp) {
#       temp_genoutput_2019_dg <- genoutput_2019_dg %>%
#         filter(region %in% temp_total) %>%
#         group_by(year,Fuel) %>%
#         summarise(AnnualOutput = sum(AnnualOutput))
#       temp_genoutput_2019_dg$case <- case_temp$case[j]
#       temp_genoutput_2019_dg <- left_join(temp_genoutput_2019_dg, cases_newnames, 
#                                           by = c('case' = 'case_description')) %>%
#         select(case, year, Fuel, Scenario, `TechSensitivity`, AnnualOutput)
#       genoutput_subregion <- rbind(genoutput_subregion,temp_genoutput_2019_dg)
#     }
#     write_csv(genoutput_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Output_w_2019',temp_total_title,".csv"))
#   }
# }


