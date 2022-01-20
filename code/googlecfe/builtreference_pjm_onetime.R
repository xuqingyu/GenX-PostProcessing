# built reference case
# read in c&i emission
library(tidyverse)
folder <- "~/Documents/PJM_Google_12x7_newruns_it2/CompiledResults/"
ci_emission <- read_csv(paste0(folder,'ci_emissions.csv')) %>%
  select(case,year,Scenario, TechSensitivity, emission_measure, `Participated Load`) %>%
  rename(ci_emission_system = emission_measure) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'))

ci_emission <- read_csv(paste0(folder,'ci_emissions.csv'));
emission_measure_bm <- ci_emission$emission_measure;
emission_measure_bm[which(ci_emission$TechSensitivity =='No 24x7 Purchase')] <- ci_emission$'emission_local_n_import'[which(ci_emission$TechSensitivity =='No 24x7 Purchase')]
ci_emission <- cbind(ci_emission, emission_measure_bm) %>%
  select(case,year,Scenario, TechSensitivity, emission_measure_bm, `Participated Load`) %>%
  rename(ci_emission_system = emission_measure_bm) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'))


ci_cost <- read_csv(paste0(folder,'ci_lse_cost.csv')) %>%
  group_by(case,year,Scenario, TechSensitivity) %>%
  summarise(value = sum(value)) %>%
  rename(ci_lsecost = value) %>%
  select(case,year,Scenario, TechSensitivity, ci_lsecost) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%')) 

systememission <- read_csv(paste0(folder,'system_emissions.csv')) %>%
  select(case,year,Scenario, TechSensitivity, emission_local_n_import) %>%
  rename(system_emission = emission_local_n_import) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'))

system_cost <- read_csv(paste0(folder,'PJM_Google/System_Cost_long_PJM_Google.csv')) %>%
  group_by(case,year,Scenario, TechSensitivity) %>%
  summarise(value = sum(value)) %>%
  rename(systemcost = value) %>%
  select(case,year,Scenario, TechSensitivity, systemcost) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%')) 

reference = ci_emission %>%
  left_join(ci_cost) %>%
  left_join(systememission) %>%
  left_join(system_cost) %>%
  select(Scenario, TechSensitivity,
         ci_emission_system,ci_lsecost,system_emission,systemcost,`Participated Load`)
write_csv(reference, 
          "/Users/qingyuxu/Dropbox (Princeton)/PJM/Project_GenX_OutputProcessing/data/Google_PJM_CFEStudy_Reference.csv")  
