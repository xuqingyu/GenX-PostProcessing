# Capacity Plot ----
# source('./code/Header.R')

gen_capacity <- read_csv(paste0(RunFdr,'/CompiledResults/capacity.csv')) %>%
  left_join(resource_mapping) %>%
  na.omit()# filter out the "resource" that are not going to show

for ( i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  
  gen_capacity_subregion <- gen_capacity %>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel) %>%
    summarize( Capacity = sum(EndCap),
               `Charging Capacity` = sum(EndChargeCap),
               `Energy Capacity` = sum(EndEnergyCap),
               `Capacity Expansion` = sum(NewCap),
               `Energy Capacity Expansion` = sum(NewEnergyCap),
               `Charging Capacity Expansion` = sum(NewChargeCap),
               `Capacity Retirement` = sum(RetCap),
               `Energy Capacity Retirement` = sum(RetEnergyCap),
               `Charging Capacity Retirement` = sum(RetChargeCap)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case, year, Fuel, Scenario, TechSensitivity, 
           Capacity, `Charging Capacity`, `Energy Capacity`,
           `Capacity Expansion`, `Energy Capacity Expansion`, `Charging Capacity Expansion`, 
           `Capacity Retirement`, `Energy Capacity Retirement`, `Charging Capacity Retirement`)
  write_csv(gen_capacity_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Capacity_',temp_total_title,".csv"))
}

for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gen_profit_subregion_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Profit_',temp_total_title,".csv")
  if (file.exists(gen_profit_subregion_fn)){
    gencapacity_subregion <- read_csv(gen_profit_subregion_fn) %>%
      select(case, year, Fuel, Scenario, TechSensitivity, Capacity)
    if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
      gencapacity_2019_base <- read_csv('./data/Gen_capacity_2019.csv') %>%
        left_join(resource_mapping) %>%
        group_by(year,zone,region,Fuel) %>%
        summarise(Capacity = sum(Capacity)) 
      case_temp <- unique(select(gencapacity_subregion,case))
      n_case_temp <- dim(case_temp)[1]
      for (j in 1:n_case_temp) {
        temp_gencapacity_2019_base <- gencapacity_2019_base %>%
          filter(region %in% temp_total) %>%
          group_by(year,Fuel) %>%
          summarise(Capacity = sum(Capacity))
        temp_gencapacity_2019_base$case <- case_temp$case[j]
        temp_gencapacity_2019_base <- left_join(temp_gencapacity_2019_base, cases_newnames, by = c('case' = 'case_description')) %>%
          select(case, year, Fuel, Scenario, `TechSensitivity`, Capacity)
        gencapacity_subregion <- rbind(gencapacity_subregion,temp_gencapacity_2019_base)
      }
    }
    if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
      gencapacity_2019_dg <- read_csv('./data/PJM_DG_2019.csv') %>%
        rename(region = `GenX.Region`,Capacity = `DG_Capacity`) %>%
        left_join(zone_mapping) %>%
        mutate(Fuel = 'DG Solar',year = 2019) %>%
        select(year,zone,region,Fuel,Capacity)
      case_temp <- unique(select(gencapacity_subregion,case))
      n_case_temp <- dim(case_temp)[1]
      for (j in 1:n_case_temp) {
        temp_gencapacity_2019_dg <- gencapacity_2019_dg %>%
          filter(region %in% temp_total) %>%
          group_by(year,Fuel) %>%
          summarise(Capacity = sum(Capacity))
        temp_gencapacity_2019_dg$case <- case_temp$case[j]
        temp_gencapacity_2019_dg <- left_join(temp_gencapacity_2019_dg, cases_newnames, by = c('case' = 'case_description')) %>%
          select(case, year, Fuel, Scenario, `TechSensitivity`, Capacity)
        gencapacity_subregion <- rbind(gencapacity_subregion,temp_gencapacity_2019_dg)
      }
    }
    if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
      gencapacity_runs_dg <- read_csv('./data/SCENARIO_DG_capacity_byRegion.csv') %>%
        filter(Year %in% years) %>%
        rename(region = `GenX.Region`,year = Year,`Capacity` = `DG_Capacity`) %>%
        rename(`SCENARIO_Load` = `SCENARIO`) %>%
        mutate(Fuel = 'DG Solar') %>%
        filter(region %in% temp_total) %>%
        group_by(year,Fuel,`SCENARIO_Load`) %>%
        summarise(Capacity = sum(Capacity))
      gencapacity_runs_dg <- left_join(load_mapping,gencapacity_runs_dg) %>%
        rename(`case` = `case_description`) %>%
        left_join(cases_newnames, by = c('case' = 'case_description')) %>%
        select(case, year, Fuel, Scenario, `TechSensitivity`, Capacity)
      gencapacity_subregion <- rbind(gencapacity_subregion, gencapacity_runs_dg)
    }
  }
  write_csv(gencapacity_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],'/Generation/Gen_Capacity_w_2019',temp_total_title,".csv"))
}


