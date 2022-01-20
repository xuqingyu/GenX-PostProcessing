
# System Output ----
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
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  select(Scenario, TechSensitivity, Fuel, `AnnualOutput`,`Target`,`Shortfall price`,`Post-Grid CFE Score Local_n_Import`) %>%
  filter(TechSensitivity != 'No 24x7 Purchase')
cfe_output_allcase_system_ref <- read_csv(paste0(temp_RunFdr,"/CompiledResults/", 
                                             subreg,"/Generation/Gen_Output_",subreg,".csv"),
                                      col_types = cols()) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  filter(year == '2030') %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  select(Scenario, TechSensitivity, Fuel, `AnnualOutput`) %>%
  filter(TechSensitivity == 'No 24x7 Purchase') %>%
  rename(RefOutput = `AnnualOutput`) %>%
  select(-TechSensitivity)



cfe_output_allcase_system_diff = full_join(cfe_output_allcase_system, 
                                           cfe_output_allcase_system_ref, 
                                           by = c('Scenario','Fuel')) %>% unique()
cfe_output_allcase_system_diff[is.na(cfe_output_allcase_system_diff)] <- 0
cfe_output_allcase_system_diff <- cfe_output_allcase_system_diff %>%
  mutate(Diff = `AnnualOutput` - RefOutput) %>%
  arrange(Target)

No24rows = which(cfe_output_allcase_system_diff$TechSensitivity == 'No 24x7 Purchase')
cfe_output_allcase_system_diff$Target[No24rows] <- cfe_output_allcase_system_diff$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_output_allcase_system_diff$TechSensitivity == 'Annual 100%')
cfe_output_allcase_system_diff$Target[Annualrows] <- cfe_output_allcase_system_diff$`Post-Grid CFE Score Local_n_Import`[Annualrows]
cfe_output_allcase_system_ref <- cfe_output_allcase_system_ref %>%
  mutate(TechSensitivity = 'No 24x7 Purchase')

cfe_output_allcase_system_diff_net <- cfe_output_allcase_system_diff %>%
  group_by(Scenario,TechSensitivity,Target,`Shortfall price`,`Post-Grid CFE Score Local_n_Import`) %>%
  summarize(Net = (-1)*sum(Diff))

if (Studyregion == 'WECC') {
  cfe_output_allcase_system_diff = cfe_output_allcase_system_diff %>%
    filter(!(Fuel == 'Adv. Nuclear'))
  output_diff_limit_5p = c(-7.5,10)
  output_diff_break_5p = seq(from = -10, to = 10, by = 2)
  output_diff_limit_10p = c(-15,20)
  output_diff_break_10p = seq(from = -20, to = 20, by = 4)
  output_diff_limit_25p = c(-20,50)
  output_diff_break_25p = seq(from = -30, to = 60, by = 10)
  output_limit = c(0,225)
  output_break = seq(from = 0, to = 225, by = 40)
} else {
  output_diff_limit_5p = c(-30,35)
  output_diff_break_5p = seq(from = -30, to = 40, by = 10)
  output_diff_limit_10p = c(-70,70)
  output_diff_break_10p = seq(from = -80, to = 100, by = 20)
  output_diff_limit_25p = c(-150,300)
  output_diff_break_25p = seq(from = -200, to = 300, by = 50)
  output_limit = c(0,1000)
  output_break = seq(from = 0, to = 1000, by = 200)
}
p_width_all = 12
p_height_all = 6

p_width_ref = 2
p_height_ref = 6


ggplot()+
  geom_col(data = filter(cfe_output_allcase_system_diff, 
                         grepl('^10%',Scenario),
                         !grepl('CES|Ex.|Hi.|45Q',Scenario)),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  geom_point(data = filter(cfe_output_allcase_system_diff_net, 
                         grepl('^10%',Scenario),
                         !grepl('CES|Ex.|Hi.|45Q',Scenario)),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = `Net`/1e6),
           colour="black", 
           size= 2) +
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('CFE Score')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  coord_cartesian(ylim = output_diff_limit_10p)+
  scale_y_continuous(breaks = output_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_10p_new.png'),
         width = 15,
         height = 5)



ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x = round(Target,2), 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = output_diff_limit_10p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = output_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_10p.png'),
         width = 4,
         height = 9)

ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x = round(Target,2), 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = output_diff_limit_5p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = output_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_5p.png'),
         width = 4,
         height = 9)
ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x = round(Target,2), 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = output_diff_limit_25p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = output_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_25p.png'),
         width = 4,
         height = 9)
ggplot(data = filter(cfe_output_allcase_system_diff,
                     grepl('^10%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x = TechSensitivity,
               y = `Diff`/1e6,
               fill=Fuel),
           colour="black",
           size= 0.1,
           width = 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('')+
  ylab('Generation Output Diff (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_diff_limit_10p)+
  scale_y_continuous(breaks = output_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_10p_annual100.png'),
         width = 4,
         height = 9)
ggplot(data = filter(cfe_output_allcase_system_diff,
                     grepl('^5%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x = TechSensitivity,
               y = `Diff`/1e6,
               fill=Fuel),
           colour="black",
           size= 0.1,
           width = 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('')+
  ylab('Generation Output Diff (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_diff_limit_5p)+
  scale_y_continuous(breaks = output_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_5p_annual100.png'),
         width = 4,
         height = 9)
ggplot(data = filter(cfe_output_allcase_system_diff,
                     grepl('^25%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x = TechSensitivity,
               y = `Diff`/1e6,
               fill=Fuel),
           colour="black",
           size= 0.1,
           width = 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('')+
  ylab('Generation Output Diff (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_diff_limit_25p)+
  scale_y_continuous(breaks = output_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_25p_annual100.png'),
         width = 4,
         height = 9)
# ggplot(data = filter(cfe_output_allcase_system_diff, 
#                      grepl('^10%',Scenario),
#                      !grepl('CES',Scenario),
#                      !grepl('Annual 100', TechSensitivity),
#                      abs(`Shortfall price`)>0))+
#   geom_col(aes(x = round(Target,2), 
#                y = `Diff`/1e6, 
#                fill=Fuel),
#            colour="black", 
#            size= 0.1) +
#   facet_wrap(Scenario~.) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   xlab('Target CFE')+
#   ylab('Generation Output Diff (TWh)') + 
#   geom_hline(yintercept = 0, color = 'grey30')+
#   coord_cartesian(xlim = c(0.8,1), ylim = output_diff_limit_10p)+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = output_diff_break_10p)+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
#                 '/Graphics/System_Output_Compared_to_noPurchase_10p.png'),
#          width = p_width_all,
#          height = p_height_all)
# 
# ggplot(data = filter(cfe_output_allcase_system_diff,
#                      grepl('^10%',Scenario),
#                      grepl('Curt.',Scenario),
#                      !grepl('CES',Scenario),
#                      grepl('Annual 100', TechSensitivity)))+
#   geom_col(aes(x = TechSensitivity,
#                y = `Diff`/1e6,
#                fill=Fuel),
#            colour="black",
#            size= 0.1,
#            width = 0.25) +
#   facet_wrap(Scenario~.) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) +
#   xlab('')+
#   ylab('Generation Output Diff (TWh)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   coord_cartesian(ylim = output_diff_limit_10p)+
#   scale_y_continuous(breaks = output_diff_break_10p)+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
#                 '/Graphics/System_Output_Compared_to_noPurchase_10p_annual100.png'),
#          width = p_width_ref,
#          height = p_height_ref)


if (Studyregion == 'WECC') {
  cfe_output_allcase_system_ref = cfe_output_allcase_system_ref %>%
    filter(!(Fuel == 'Adv. Nuclear'))
}
ggplot(data = filter(cfe_output_allcase_system_ref, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Ex.|Hi.|45Q',Scenario),
                     !grepl('CES',Scenario),
                     grepl('Curt.',Scenario)))+
  geom_col(aes(x= TechSensitivity, 
               y = `RefOutput`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1, 
           width = .5) +
  facet_wrap(Scenario~., ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Output (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_limit)+
  scale_y_continuous(breaks = output_break)+
  # coord_cartesian(ylim = c(0,55))+
  # scale_y_continuous(breaks = seq(from = 0, to = 1000, by = 200))+
  theme_bw() +
  theme(legend.position = 'none')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Reference_10p.png'),
         width = 1,
         height = 4)

ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x = round(Target,2), 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = output_diff_limit_10p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = output_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_10p_ces.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^10%',Scenario),
                     grepl('Curt.',Scenario),
                     grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x = TechSensitivity, 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1, 
           width = 0.25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_diff_limit_10p)+
  scale_y_continuous(breaks = output_diff_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_10p_annual100_ces.png'),
         width = p_width_ref,
         height = p_height_ref)


if (Studyregion == 'WECC') {
  cfe_output_allcase_system_ref = cfe_output_allcase_system_ref %>%
    filter(!(Fuel == 'Nuclear'))
}
ggplot(data = filter(cfe_output_allcase_system_ref, 
                     grepl('^10%',Scenario),
                     grepl('Curt.',Scenario),
                     grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, 
               y = `RefOutput`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1, 
           width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Output (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_limit)+
  scale_y_continuous(breaks = output_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Reference_10p_ces.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x = round(Target,2), 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = output_diff_limit_5p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = output_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_5p.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^5%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x = TechSensitivity, 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1, 
           width = 0.25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_diff_limit_5p)+
  scale_y_continuous(breaks = output_diff_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_5p_annual100.png'),
         width = p_width_ref,
         height = p_height_ref)


if (Studyregion == 'WECC') {
  cfe_output_allcase_system_ref = cfe_output_allcase_system_ref %>%
    filter(!(Fuel == 'Nuclear'))
}
ggplot(data = filter(cfe_output_allcase_system_ref, 
                     grepl('^5%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, 
               y = `RefOutput`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1, 
           width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Output (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_limit)+
  scale_y_continuous(breaks = output_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Reference_5p.png'),
         width = p_width_ref,
         height = p_height_ref)


ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('Annual 100', TechSensitivity),
                     abs(`Shortfall price`)>0))+
  geom_col(aes(x = round(Target,2), 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('Target CFE')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = output_diff_limit_25p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = output_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_25p.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_output_allcase_system_diff, 
                     grepl('^25%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario),
                     grepl('Annual 100', TechSensitivity)))+
  geom_col(aes(x = TechSensitivity, 
               y = `Diff`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1, 
           width = 0.25) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Generation Output Diff (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_diff_limit_25p)+
  scale_y_continuous(breaks = output_diff_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Compared_to_noPurchase_25p_annual100.png'),
         width = p_width_ref,
         height = p_height_ref)


if (Studyregion == 'WECC') {
  cfe_output_allcase_system_ref = cfe_output_allcase_system_ref %>%
    filter(!(Fuel == 'Nuclear'))
}
ggplot(data = filter(cfe_output_allcase_system_ref, 
                     grepl('^25%',Scenario),
                     grepl('Curt.',Scenario),
                     !grepl('CES',Scenario)))+
  geom_col(aes(x= TechSensitivity, 
               y = `RefOutput`/1e6, 
               fill=Fuel),
           colour="black", 
           size= 0.1, 
           width = 0.25) +
  facet_wrap(Scenario~., nrow = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Reference Output (TWh)') + 
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = output_limit)+
  scale_y_continuous(breaks = output_break)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,
                '/Graphics/System_Output_Reference_25p.png'),
         width = p_width_ref,
         height = p_height_ref)

