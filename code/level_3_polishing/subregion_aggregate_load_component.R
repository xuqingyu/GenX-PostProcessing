# Calculate gross load in TWh----
source('./code/Header.R')
load_twh_fn <- paste0(RunFdr,'/Total_load_by_region_DG_subtracted_summary.csv')
# load_twh <- read_csv(load_twh_fn, col_types = cols()) %>%
#   pivot_wider(id_cols = c(SCENARIO,Year,GenX.Region),names_from = 'Load_Type',values_from = 'TWh') %>%
#   filter(Year %in% years) %>%
#   rename(region = `GenX.Region`,year = Year) %>%
#   left_join(zone_mapping) %>%
#   rename(`SCENARIO_Load` = `SCENARIO`) %>%
#   na.omit();
load_twh <- read_csv(load_twh_fn, col_types = cols()) %>%
  # pivot_wider(id_cols = c(SCENARIO,Year,GenX.Region),names_from = 'Load_Type',values_from = 'TWh') %>%
  filter(Year %in% years) %>%
  rename(region = `GenX.Region`,year = Year) %>%
  left_join(zone_mapping) %>%
  rename(`SCENARIO_Load` = `SCENARIO`) %>%
  na.omit();
load_runs <- read_csv(paste0(RunFdr,'/CompiledResults/Total_load_summary.csv'), col_types = cols()) %>%
  mutate(Region = str_remove(Region,'Load_MW_z')) %>%
  rename(zone = Region) %>%
  left_join(load_mapping, by = c('case' = 'case_description'))

if ((temp_total_title == 'New Jersey')|(temp_total_title == 'PJM')) {
  load_combined <- left_join(load_runs, load_twh) %>%
    mutate(dg_TWh = (-1)*dg_TWh + dg_TWh_curtail,
           AnnualLoad = round(AnnualLoad, digits = 2),
           `Gross Total` = base_TWh + water_heat_TWh + space_heat_TWh + ldev_TWh + mhbev_TWh) %>%
    select(case,year,zone,region, base_TWh, water_heat_TWh,space_heat_TWh, ldev_TWh, mhbev_TWh, `Gross Total`, dg_TWh, Total_TWh, AnnualLoad) %>%
    rename(`Other Load` = `base_TWh`,
           `Com&Res Water Heating Load` = water_heat_TWh,
           `Com&Res Space Heating & Cooling Load` = space_heat_TWh,
           `Trans LDV Load` = ldev_TWh,
           `Trans MDV/HDV/Bus Load` = mhbev_TWh,
           `Distributed Solar` = dg_TWh,
           `Total` = Total_TWh,
           `Total (after Temporal Resolution Reduction)` = AnnualLoad) %>%
    pivot_longer(cols=!c(case,year,zone,region),names_to = 'Load Type',values_to = 'TWh')
} else {
  load_combined <- left_join(load_runs, load_twh) %>%
  mutate(DG_TWh = (-1)*DG_TWh + DG_curtail_TWh,
         AnnualLoad = round(AnnualLoad, digits = 2),
         `Gross Total` = (Com_Other_TWh + Com_SPH_TWh + Com_WH_TWh + Ind_TWh + 
                            Res_Other_TWh + Res_SPH_TWh + Res_WH_TWh + 
                            Trans_HDV_TWh + Trans_LDV_TWh + Trans_MDV_TWh + Trans_Other_TWh)) %>%
  select(case,year,zone,region, Com_Other_TWh, Com_SPH_TWh,Com_WH_TWh, Ind_TWh, 
         Res_Other_TWh, Res_SPH_TWh, Res_WH_TWh, 
         Trans_HDV_TWh, Trans_LDV_TWh, Trans_MDV_TWh, Trans_Other_TWh,
         `Gross Total`, DG_TWh, Total_TWh, AnnualLoad) %>%
  rename(`Commercial Other Load` = Com_Other_TWh,
         `Commercial Space Heating & Cooling Load` = Com_SPH_TWh,
         `Commercial Water Heating Load` = Com_WH_TWh,
         `Industrial Load` = Ind_TWh,
         `Residential Other Load` = Res_Other_TWh,
         `Residential Space Heating & Cooling Load` = Res_SPH_TWh,
         `Residential Water Heating Load` = Res_WH_TWh,
         `Trans LDV Load` = Trans_LDV_TWh,
         `Trans MDV Load` = Trans_MDV_TWh,
         `Trans HDV Load` = Trans_HDV_TWh,
         `Trans Other Load` = Trans_Other_TWh,
         `Distributed Solar` = DG_TWh,
         `Total` = Total_TWh,
         `Total (after Temporal Resolution Reduction)` = AnnualLoad) %>%
  pivot_longer(cols=!c(case,year,zone,region),names_to = 'Load Type',values_to = 'TWh')
  }




for (i in 1:n_subregions){
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  temp_load_combined <- load_combined %>%
    filter(region %in% temp_total) %>%
    group_by(case, year,`Load Type`) %>%
    summarise(TWh = sum(TWh)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Scenario, TechSensitivity, `Load Type`,TWh)
  write_csv(temp_load_combined, paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Load/Load_Component_",Subregions[i],".csv"))
}
rm(load_twh_fn, load_twh, load_combined,load_runs,temp_load_combined)



