# Capacity Price ----
# source('./code/Header.R')
capprice_fn <- paste0(RunFdr,'/CompiledResults/Res_Mar.csv')
if (file.exists(capprice_fn)) {
  capprice <- read_csv(capprice_fn, col_types = cols()) %>%
    group_by(case,year,name) %>%
    summarise(`Capacity Price` = round(sum(value*weight)/365,digits=3)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,name,Scenario,TechSensitivity,`Capacity Price`) %>%
    write_csv(paste0(RunFdr,'/CompiledResults/Capacity_Price.csv'))
} else {
  print('there is no Res_Mar.csv')
}
