engprice_fn <- paste0(RunFdr,'/CompiledResults/price_summary.csv')
if (file.exists(engprice_fn)) {
  engprice <- read_csv(engprice_fn) %>%
    group_by(case,year,name) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,name,Scenario,TechSensitivity,`AnnualPrice`) %>%
    write_csv(paste0(RunFdr,'/CompiledResults/Energy_Price.csv'))
} else {
  print('there is no price_summary.csv')
}
