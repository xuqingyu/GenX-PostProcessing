# System capacity ----
cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))
cfe_capacity_allcase_system <- read_csv(paste0(temp_RunFdr,"/CompiledResults/",subreg,
                                               "/Generation/Gen_Capacity_",subreg,".csv"),
                                        col_types = cols()) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  filter(year == '2030') %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  select(Scenario, TechSensitivity, Fuel, `Capacity`,`Target`,`Shortfall price`,`Post-Grid CFE Score Local_n_Import`) %>%  
  filter(TechSensitivity != 'No 24x7 Purchase')

cfe_capacity_allcase_system_ref <- read_csv(paste0(temp_RunFdr,"/CompiledResults/",subreg,
                                               "/Generation/Gen_Capacity_",subreg,".csv"),
                                        col_types = cols()) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  filter(year == '2030') %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  select(Scenario, TechSensitivity, Fuel, `Capacity`) %>%
  rename(RefCapacity = `Capacity`) %>%
  filter(TechSensitivity == 'No 24x7 Purchase') %>%
  select(-TechSensitivity)


cfe_capacity_allcase_system_diff = full_join(cfe_capacity_allcase_system, 
                                             cfe_capacity_allcase_system_ref, 
                                             by = c('Scenario','Fuel')) %>% unique()
cfe_capacity_allcase_system_diff[is.na(cfe_capacity_allcase_system_diff)] <- 0
cfe_capacity_allcase_system_diff <- cfe_capacity_allcase_system_diff %>%
  mutate(Diff = `Capacity` - RefCapacity) %>%
  arrange(Target)

No24rows = which(cfe_capacity_allcase_system_diff$TechSensitivity == 'No 24x7 Purchase')
cfe_capacity_allcase_system_diff$Target[No24rows] <- cfe_capacity_allcase_system_diff$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_capacity_allcase_system_diff$TechSensitivity == 'Annual 100%')
cfe_capacity_allcase_system_diff$Target[Annualrows] <- cfe_capacity_allcase_system_diff$`Post-Grid CFE Score Local_n_Import`[Annualrows]



cfe_capacity_allcase_system_ref <- cfe_capacity_allcase_system_ref %>%
  mutate(TechSensitivity = 'No 24x7 Purchase')
p_width_all = 12
p_height_all = 6

p_width_ref = 2
p_height_ref = 6

if (Studyregion == 'WECC') {
  cfe_capacity_allcase_system_diff = cfe_capacity_allcase_system_diff %>%
    filter(!(Fuel == 'Adv. Nuclear'),!(Fuel == 'LDS Hydrogen'))
  cap_diff_limit_5p = c(-1.4,2)
  cap_diff_break_5p = seq(from = -2, to = 2, by = 1)
  cap_diff_limit_10p = c(-2.5,5)
  cap_diff_break_10p = seq(from = -20, to = 100, by = 1)
  cap_diff_limit_25p = c(-5,25)
  cap_diff_break_25p = seq(from = -10, to = 25, by = 5)
  cap_limit = c(0,100)
  cap_break = seq(from = 0, to = 100, by = 20)
} else {
  cfe_capacity_allcase_system_diff = cfe_capacity_allcase_system_diff %>%
    filter(!(Fuel == 'LDS Hydrogen'))
  cap_diff_limit_5p = c(-4,20)
  cap_diff_break_5p = seq(from = -10, to = 20, by = 4)
  cap_diff_limit_10p = c(-15,50)
  cap_diff_break_10p = seq(from = -20, to = 100, by = 10)
  cap_diff_limit_25p = c(-40,120)
  cap_diff_break_25p = seq(from = -80, to = 120, by = 40)
  cap_limit = c(0,300)
  cap_break = seq(from = 0, to = 300, by = 40)
}

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario)))+
  geom_col(aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.,ncol=3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('CFE Score')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  coord_cartesian(ylim = cap_diff_limit_10p)+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  guides(fill=guide_legend(nrow=3)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_10p_new.png'),
         width = 15,
         height = 5)



ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.,ncol=1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_10p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_10p.png'),
         width = 4,
         height = 9)


ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.,ncol=1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_5p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_5p.png'),
         width = 4,
         height = 9)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.,ncol=1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_25p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_25p.png'),
         width = 4,
         height = 9)

# ggplot(data = filter(cfe_capacity_allcase_system_diff, 
#                      grepl('^10%',Scenario),
#                      !grepl('CES',Scenario),
#                      !grepl('Annual 100', TechSensitivity),
#                      abs(`Shortfall price`)>0))+
#   geom_col(aes(x= round(Target,2), 
#                y = `Diff`/1000, 
#                fill=Fuel),
#            colour="black", 
#            size= 0.1) +
#   facet_wrap(Scenario~.) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   xlab('Target CFE')+
#   ylab('Capacity Diff. (GW)') + 
#   geom_hline(yintercept = 0, color = 'grey30')+
#   coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_10p)+
#   scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
#   scale_y_continuous(breaks = cap_diff_break_10p)+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_10p.png'),
#          width = p_width_all,
#          height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_10p)+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_10p_annual100.png'),
         width = 4,
         height = 9)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_5p)+
  scale_y_continuous(breaks = cap_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_5p_annual100.png'),
         width = 4,
         height = 9)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_25p)+
  scale_y_continuous(breaks = cap_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_25p_annual100.png'),
         width = 4,
         height = 9)


ggplot(data = filter(cfe_capacity_allcase_system_ref, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     grepl('Curt.',Scenario)))+
  geom_col(aes(x= TechSensitivity, y = `RefCapacity`/1000, fill=Fuel),
           colour="black", size= 0.1, width = .5) +
  facet_wrap(Scenario~., ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Capacity (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit)+
  scale_y_continuous(breaks = cap_break)+
  theme_bw() +
  theme(legend.position = 'none')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Reference_10p.png'),
         width = 1,
         height = 4)


ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_10p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_10p_ces.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     grepl('Curt.',Scenario),
                     grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_10p)+
  scale_y_continuous(breaks = cap_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_10p_annual100_ces.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_capacity_allcase_system_ref, 
                     grepl('^10%',Scenario),
                     grepl('Curt.',Scenario),
                     grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, y = `RefCapacity`/1000, fill=Fuel),
           colour="black", size= 0.1, width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Capacity (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit)+
  scale_y_continuous(breaks = cap_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Reference_10p_ces.png'),
         width = p_width_ref,
         height = p_height_ref)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_5p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_5p.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_5p)+
  scale_y_continuous(breaks = cap_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_5p_annual100.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_capacity_allcase_system_ref, 
                     grepl('^5%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, y = `RefCapacity`/1000, fill=Fuel),
           colour="black", size= 0.1, width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Capacity (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit)+
  scale_y_continuous(breaks = cap_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Reference_5p.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x= round(Target,2), 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_diff_limit_25p)+
  scale_x_continuous(breaks = seq(from =0.8, to = 1, by =0.02))+
  scale_y_continuous(breaks = cap_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_25p.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x= TechSensitivity, 
               y = `Diff`/1000, 
               fill=Fuel),
           colour="black", 
           size= 0.1,
           width = .25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity Diff. (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_diff_limit_25p)+
  scale_y_continuous(breaks = cap_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Compared_to_noPurchase_25p_annual100.png'),
         width = p_width_ref,
         height = p_height_ref)



ggplot(data = filter(cfe_capacity_allcase_system_ref, 
                     grepl('^25%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, y = `RefCapacity`/1000, fill=Fuel),
           colour="black", size= 0.1, width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Capacity (GW)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit)+
  scale_y_continuous(breaks = cap_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Capacity_Reference_25p.png'),
         width = p_width_ref,
         height = p_height_ref)
