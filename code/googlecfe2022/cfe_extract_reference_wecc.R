# built reference case
# read in c&i emission
library(tidyverse)
folder <- "~/Documents/WECC_247/CompiledResults/"
ci_emission <- read_csv(paste0(folder,'tfs_part_emissions.csv')) %>%
  select(case,year,Scenario, TechSensitivity, emission_measure, `Participated Load`) %>%
  rename(ci_emission_system = emission_measure) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'))

# ci_emission <- read_csv(paste0(folder,'ci_emissions.csv'));
# emission_measure_bm <- ci_emission$emission_measure;
# emission_measure_bm[which(ci_emission$TechSensitivity =='No 24x7 Purchase')] <- ci_emission$'emission_local_n_import'[which(ci_emission$TechSensitivity =='No 24x7 Purchase')]
# ci_emission <- cbind(ci_emission, emission_measure_bm) %>%
#   select(case,year,Scenario, TechSensitivity, emission_measure_bm, `Participated Load`) %>%
#   rename(ci_emission_system = emission_measure_bm) %>%
#   filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'))


ci_cost <- read_csv(paste0(folder,'tfs_lse_cost.csv'), col_types = cols()) %>%
  group_by(case, year, Scenario, TechSensitivity, Policy) %>%
  summarise(value = sum(value)) %>%
  rename(ci_lsecost = value) %>%
  select(case,year,Scenario, TechSensitivity, Policy, ci_lsecost) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%')) 

systememission <- read_csv(paste0(folder,'tfs_system_emissions.csv'), col_types = cols()) %>%
  select(case,year,Scenario, TechSensitivity, emission_local_n_import) %>%
  rename(system_emission = emission_local_n_import) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'))

system_cost <- read_csv(paste0(folder,'California/System_Cost_long_California.csv'), col_types = cols()) %>%
  group_by(case,year,Scenario, TechSensitivity) %>%
  summarise(value = sum(value)) %>%
  rename(systemcost = value) %>%
  select(case,year,Scenario, TechSensitivity, systemcost) %>%
  filter(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%')) 


reference = ci_emission %>%
  left_join(ci_cost) %>%
  left_join(systememission) %>%
  left_join(system_cost) %>%
  select(Scenario, TechSensitivity, Policy,
         ci_emission_system,ci_lsecost,system_emission,systemcost,`Participated Load`)
write_csv(reference, 
          "/Users/qingyuxu/Dropbox (Princeton)/PJM/Project_GenX_OutputProcessing/data/Google_WECC_CFEStudy_Reference_2022.csv")  
rm(ci_emission, ci_cost, systememission, system_cost, reference)