gen_curtail <- read_csv(paste0(RunFdr,'CompiledResults/renewablecurtail.csv'), 
                      col_types = cols()) %>%
  left_join(resource_mapping) %>%
  filter(!(Fuel %in% storage_fuel),
         grepl('Solar|Wind', Fuel)) %>%
  na.omit()
for (i in 1:n_subregions) {
  temp_total_title <- Subregions[i]
  temp_total <- Subregion_zones$Subregion_zones[Subregion_zones$Subregions == Subregions[i]]
  gencurtail_subregion <- gen_curtail%>%
    filter(Region %in% temp_total) %>%
    group_by(case,year,Fuel) %>%
    summarize(AnnualCurtail = sum(AnnualSum)) %>%
    left_join(cases_newnames, by = c('case' = 'case_description')) %>%
    select(case,year,Fuel, Scenario, `TechSensitivity`,
           AnnualCurtail)
  write_csv(gencurtail_subregion,paste0(RunFdr,'/CompiledResults/',Subregions[i],
                                       '/Generation/Gen_Curltailment_',temp_total_title,".csv"))
}
