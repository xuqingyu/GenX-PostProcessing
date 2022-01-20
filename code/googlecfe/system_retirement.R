# System retirement ----

cfe_capacity_allcase_system <- read_csv(paste0(temp_RunFdr,"/CompiledResults/",subreg,
                                               "/Generation/Gen_Capacity_",subreg,".csv"),
                                        col_types = cols()) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  filter(year == '2030') %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  select(Scenario, TechSensitivity, Fuel, `Capacity Retirement`,`Target`,`Shortfall price`) %>%  
  filter(TechSensitivity != 'No 24x7 Purchase')
cfe_capacity_allcase_system_ref <- read_csv('./data/Google_CFEStudy_Capacity_Reference.csv',
                                            col_types = cols()) %>%
  filter(Studyregion_table == Studyregion) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  select(Scenario, TechSensitivity, Fuel, `Capacity Retirement`) %>%
  rename(RefRetirement = `Capacity Retirement`) %>%
  select(-TechSensitivity)


cfe_capacity_allcase_system_diff = full_join(cfe_capacity_allcase_system, 
                                             cfe_capacity_allcase_system_ref, 
                                             by = c('Scenario','Fuel')) %>% unique()
cfe_capacity_allcase_system_diff[is.na(cfe_capacity_allcase_system_diff)] <- 0
cfe_capacity_allcase_system_diff <- cfe_capacity_allcase_system_diff %>%
  mutate(Diff = `Capacity Retirement` - RefRetirement)
cfe_capacity_allcase_system_ref <- cfe_capacity_allcase_system_ref %>%
  mutate(TechSensitivity = 'No 24x7 Purchase')
p_width_all = 12
p_height_all = 6

p_width_ref = 2
p_height_ref = 6

if (Studyregion == 'WECC') {
  cfe_capacity_allcase_system_diff = cfe_capacity_allcase_system_diff %>%
    filter(!(Fuel == 'Nuclear'))
  cap_diff_limit_5p = c(-1,1)
  cap_diff_break_5p = seq(from = -1, to = 1, by = 0.4)
  cap_diff_limit_10p = c(-1,1)
  cap_diff_break_10p = seq(from = -1, to = 1, by = 0.2)
  cap_diff_limit_25p = c(-4,1)
  cap_diff_break_25p = seq(from = -4, to = 1, by = 0.4)
  cap_limit = c(-5,0)
  cap_break = seq(from = -5, to = 0, by = 1)
} else {
  cfe_capacity_allcase_system_diff = cfe_capacity_allcase_system_diff %>%
    filter(!(Fuel == 'Retrofit Gas CC w/CCS'))
  cap_diff_limit_5p = c(-1,1)
  cap_diff_break_5p = seq(from = -1, to = 1, by = 0.4)
  cap_diff_limit_10p = c(-1,1)
  cap_diff_break_10p = seq(from = -1, to = 1, by = 0.4)
  cap_diff_limit_25p = c(-1.5,1)
  cap_diff_break_25p = seq(from = -2, to = 2, by = 0.4)
  cap_limit = c(-80,0)
  cap_break = seq(from = -80, to = 100, by = 10)
}
ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = -1*`Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Retirement Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_10p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Compared_to_noPurchase_10p.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = -1*`Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Retirement Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_10p)+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Compared_to_noPurchase_10p_annual100.png'),
         width = p_width_ref,
         height = p_height_ref)



ggplot(data = filter(cfe_capacity_allcase_system_ref, 
                     grepl('^10%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, y = -1*`RefRetirement`/1000, fill=Fuel),
           colour="black", size= 0.1, width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Retirement (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit)+
  scale_y_continuous(breaks = cap_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Reference_10p.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = -1*`Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Retirement Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_10p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Compared_to_noPurchase_10p_ces.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     grepl('Curt.',Scenario),
                     grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = -1*`Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Retirement Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_10p)+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Compared_to_noPurchase_10p_annual100_ces.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_capacity_allcase_system_ref, 
                     grepl('^10%',Scenario),
                     grepl('Curt.',Scenario),
                     grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, y = -1*`RefRetirement`/1000, fill=Fuel),
           colour="black", size= 0.1, width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Retirement (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit)+
  scale_y_continuous(breaks = cap_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Reference_10p_ces.png'),
         width = p_width_ref,
         height = p_height_ref)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = -1*`Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Retirement Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_5p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Compared_to_noPurchase_5p.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = -1*`Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Retirement Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_5p)+
  scale_y_continuous(breaks = cap_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Compared_to_noPurchase_5p_annual100.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_capacity_allcase_system_ref, 
                     grepl('^5%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, y = -1*`RefRetirement`/1000, fill=Fuel),
           colour="black", size= 0.1, width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Retirement (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit)+
  scale_y_continuous(breaks = cap_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Reference_5p.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = -1*`Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Retirement Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_25p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Compared_to_noPurchase_25p.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = -1*`Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Retirement Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_25p)+
  scale_y_continuous(breaks = cap_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Compared_to_noPurchase_25p_annual100.png'),
         width = p_width_ref,
         height = p_height_ref)



ggplot(data = filter(cfe_capacity_allcase_system_ref, 
                     grepl('^25%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, y = -1*`RefRetirement`/1000, fill=Fuel),
           colour="black", size= 0.1, width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Retirement (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit)+
  scale_y_continuous(breaks = cap_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Retirement_Reference_25p.png'),
         width = p_width_ref,
         height = p_height_ref)

