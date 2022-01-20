cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))

cfe_emission_table_withname = read_csv(paste0(temp_RunFdr,
                                              '/CompiledResults/system_emissions.csv'), 
                                       col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list), 
         Scenario = factor(Scenario, levels = y_list)) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity'))
if (Studyregion == 'WECC') {
  emission_limit = c(30,50)
  emission_limit_ces = c(0,20)
  emission_break = seq(from = 0, to = 50, by = 2)
} else {
  emission_limit = c(150,230)
  emission_limit_ces = c(0,80)
  emission_break = seq(from = 0, to = 300, by = 10)
}

No24rows = which(cfe_emission_table_withname$TechSensitivity == 'No 24x7 Purchase')
cfe_emission_table_withname$Target[No24rows] <- cfe_emission_table_withname$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_emission_table_withname$TechSensitivity == 'Annual 100%')
cfe_emission_table_withname$Target[Annualrows] <- cfe_emission_table_withname$`Post-Grid CFE Score Local_n_Import`[Annualrows]

cfe_emission_table_withname <- cfe_emission_table_withname %>%
  arrange(Target)

cfe_emission_table_withname_with_ref <- cfe_emission_table_withname %>%
  left_join(select(filter(reference,TechSensitivity == 'No 24x7 Purchase'), Scenario, system_emission)) %>%
  mutate(AvoidedEmission = (system_emission - emission_local_n_import)/1e6)


cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))
cfe_gen_allcase <- read_csv(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Generation/CFE_Gen_Output_",subreg,".csv")) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(`Post-Grid CFE Score Local_n_Import` = round(as.numeric(`Post-Grid CFE Score Local_n_Import`),3)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  na.omit()

No24rows = which(cfe_gen_allcase$TechSensitivity == 'No 24x7 Purchase')
cfe_gen_allcase$Target[No24rows] <- cfe_gen_allcase$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_gen_allcase$TechSensitivity == 'Annual 100%')
cfe_gen_allcase$Target[Annualrows] <- cfe_gen_allcase$`Post-Grid CFE Score Local_n_Import`[Annualrows]

cfe_gen_allcase <- cfe_gen_allcase %>%
  arrange(Target)
cfe_gen_allcase_total <- cfe_gen_allcase %>%
  group_by(Scenario, TechSensitivity)%>%
  summarize(TotalAnnualOutput_GWh = sum(AnnualOutput)/1e6)


nomalizedavoidedemission = left_join(cfe_emission_table_withname_with_ref, cfe_gen_allcase_total) %>%
  mutate(normalized_avoidedemission = AvoidedEmission/TotalAnnualOutput_GWh) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list), 
         Scenario = factor(Scenario, levels = y_list))


ggplot(data = filter(nomalizedavoidedemission, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario),
                     TechSensitivity != 'No 24x7 Purchase'))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = normalized_avoidedemission,
               fill = Scenario),
           colour="black", size= 0.1,
           position = 'dodge') +
  scale_fill_brewer(palette = 'Set1') +
  xlab('CFE Score')+
  ylab('Normalized Avoided Emission (ton/MWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_y_continuous(limits = c(0,0.3),breaks = seq(0,0.3,0.1))+
  theme_bw() +
  theme(legend.position = 'none')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_nomalizedemission_10p.png'),
         width = 6,
         height = 6)

ggplot(data = filter(nomalizedavoidedemission, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Hi.|45Q',Scenario),
                     TechSensitivity != 'No 24x7 Purchase'))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = normalized_avoidedemission,
               fill = Scenario),
           colour="black", size= 0.1,
           position = 'dodge') +
  scale_fill_brewer(palette = 'Set1') +
  xlab('CFE Score')+
  ylab('Normalized Avoided Emission (ton/MWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_y_continuous(limits = c(0,0.3),breaks = seq(0,0.3,0.1))+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_nomalizedemission_10p_withex.png'),
         width = 6,
         height = 6)
