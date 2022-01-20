# Curtailment checking

cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))

cfe_output_allcase_system <- read_csv(paste0(temp_RunFdr,"/CompiledResults/", 
                                             subreg,"/Generation/Gen_Output_",subreg,".csv"),
                                      col_types = cols()) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  filter(year == '2030') %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))

cfe_curtailment_allcase_system <- read_csv(paste0(temp_RunFdr,"/CompiledResults/", 
                                             subreg,"/Generation/Gen_Curltailment_",subreg,".csv"),
                                      col_types = cols()) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  filter(year == '2030') %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))


curtail_calculation = cfe_output_allcase_system %>%
  left_join(cfe_curtailment_allcase_system) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(curtailmentratio = round((AnnualCurtail/(AnnualOutput+AnnualCurtail) ),2))%>%
  select(Scenario, TechSensitivity, Fuel, `AnnualOutput`, `AnnualCurtail`, curtailmentratio,`Target`,`Shortfall price`,`Post-Grid CFE Score Local_n_Import`) %>%
  na.omit()
curtail_calculation$curtailmentratio[which(curtail_calculation$AnnualOutput<100)] <- 0


No24rows = which(curtail_calculation$TechSensitivity == 'No 24x7 Purchase')
curtail_calculation$Target[No24rows] <- curtail_calculation$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(curtail_calculation$TechSensitivity == 'Annual 100%')
curtail_calculation$Target[Annualrows] <- curtail_calculation$`Post-Grid CFE Score Local_n_Import`[Annualrows]

curtail_calculation <- curtail_calculation %>%
  arrange(Target)

modifiedrow = which((curtail_calculation$TechSensitivity=='Annual 100%')&
                      (curtail_calculation$Scenario=='10% CI Part., Curt. Tech., 80% CES'))
curtail_calculation$Target[modifiedrow] = curtail_calculation$Target[modifiedrow] - 0.01
modifiedrow = which((curtail_calculation$TechSensitivity=='Annual 100%')&
                      (curtail_calculation$Scenario=='10% CI Part., Adv. Tech. Full, 80% CES'))
curtail_calculation$Target[modifiedrow] = curtail_calculation$Target[modifiedrow] - 0.01
modifiedrow = which((curtail_calculation$TechSensitivity=='Annual 100%')&
                      (curtail_calculation$Scenario=='10% CI Part., Adv. Tech. no Comb., 80% CES'))
curtail_calculation$Target[modifiedrow] = curtail_calculation$Target[modifiedrow] - 0.01



ggplot()+
  geom_col(data = filter(curtail_calculation, 
                         grepl('^10%',Scenario),
                         !grepl('CES|Ex.|Hi.|45Q',Scenario)),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = curtailmentratio, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           position = 'dodge') +
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('CFE Score')+
  ylab('Curtailment Ratio') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Curtailment_10p_new.png'),
         width = 15,
         height = 5)


ggplot()+
  geom_col(data = filter(curtail_calculation, 
                         grepl('^10%',Scenario),
                         grepl('CES',Scenario),
                         (abs(`Shortfall price`)>0)|(TechSensitivity %in% c('No 24/7 Purchase','Annual 100%'))),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = curtailmentratio, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           position = 'dodge') +
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('CFE Score')+
  ylab('Curtailment Ratio') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Curtailment_10p_new_ces.png'),
         width = 15,
         height = 5)
