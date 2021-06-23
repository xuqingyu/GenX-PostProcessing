# Transmission Expansion ----
source('./code/Header.R')
trans_exp <- read_csv(paste0(RunFdr,"/CompiledResults/trans.csv")) %>%
  left_join(cases_newnames,by = c('case' = 'case_description')) %>%
  left_join(trans_line_name, by = c('Line' = 'Network_lines')) %>%
  select(case,year,Line,`Transmission Path Name`,Scenario, TechSensitivity,New_Trans_Capacity) %>%
  write_csv(paste0(RunFdr,"/CompiledResults/TransExpansion.csv"))


case_temp <- unique(select(trans_exp,case))
n_case_temp <- dim(case_temp)[1]
trans_2019 <- read_csv('./data/Trans_Capacity_2019.csv')
trans_cap <- trans_exp;
for (j in 1:n_case_temp) {
  temp_trans_2019 <- trans_2019 %>%
    rename(`Line` = `Network_lines`,`New_Trans_Capacity` = `Trans Capacity`);
  temp_trans_2019$case <- case_temp$case[j]
  temp_trans_2019 <- left_join(temp_trans_2019, cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Line,`Transmission Path Name`,Scenario, TechSensitivity,New_Trans_Capacity)
  trans_cap <- rbind(trans_cap,temp_trans_2019)
}
if (years[1] == '2030'){
  trans_cap <- trans_cap %>%
    pivot_wider(names_from = year, values_from = New_Trans_Capacity) %>%
    mutate(`2030` = `2030` + `2019`) %>%
    mutate(`2040` = `2040` + `2030`) %>%
    mutate(`2050` = `2050` + `2040`) %>%
    pivot_longer(cols = c(`2019`,`2030`,`2040`,`2050`),names_to = 'year',values_to = 'Capacity')
} else if (years[1] == '2022') {
  trans_cap <- trans_cap %>%
    pivot_wider(names_from = year, values_from = New_Trans_Capacity) %>%
    mutate(`2022` = `2022` + `2019`) %>%
    mutate(`2025` = `2025` + `2022`) %>%
    mutate(`2030` = `2030` + `2025`) %>%
    pivot_longer(cols = c(`2019`,`2022`,`2025`,`2030`),names_to = 'year',values_to = 'Capacity')
}
trans_cap <- trans_cap %>% select(case,year,Line,`Transmission Path Name`,Scenario, TechSensitivity,Capacity)
write_csv(trans_cap, paste0(RunFdr,"/CompiledResults/TransCapacity.csv"))
