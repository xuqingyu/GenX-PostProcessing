# Capacity plot ----
cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))
cfe_capacity_allcase <- read_csv(paste0(temp_RunFdr,"/CompiledResults/",subreg,
                                        "/Generation/CFE_Gen_Capacity_",subreg,".csv")) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(`Post-Grid CFE Score Local_n_Import` = round(as.numeric(`Post-Grid CFE Score Local_n_Import`),3)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  na.omit()

No24rows = which(cfe_capacity_allcase$TechSensitivity == 'No 24x7 Purchase')
cfe_capacity_allcase$Target[No24rows] <- cfe_capacity_allcase$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_capacity_allcase$TechSensitivity == 'Annual 100%')
cfe_capacity_allcase$Target[Annualrows] <- cfe_capacity_allcase$`Post-Grid CFE Score Local_n_Import`[Annualrows]

cfe_capacity_allcase <- cfe_capacity_allcase %>%
  arrange(Target)
modifiedrow = which((cfe_capacity_allcase$TechSensitivity=='Annual 100%')&
                      (cfe_capacity_allcase$Scenario=='10% CI Part., Curt. Tech., 80% CES'))
cfe_capacity_allcase$Target[modifiedrow] = cfe_capacity_allcase$Target[modifiedrow] - 0.01
modifiedrow = which((cfe_capacity_allcase$TechSensitivity=='Annual 100%')&
                      (cfe_capacity_allcase$Scenario=='10% CI Part., Adv. Tech. Full, 80% CES'))
cfe_capacity_allcase$Target[modifiedrow] = cfe_capacity_allcase$Target[modifiedrow] - 0.01
modifiedrow = which((cfe_capacity_allcase$TechSensitivity=='Annual 100%')&
                      (cfe_capacity_allcase$Scenario=='10% CI Part., Adv. Tech. no Comb., 80% CES'))
cfe_capacity_allcase$Target[modifiedrow] = cfe_capacity_allcase$Target[modifiedrow] - 0.01


p_width_all = 12
p_height_all = 6

p_width_ref = 4
p_height_ref = 6
if (Studyregion == 'WECC') {
  cfe_capacity_allcase = cfe_capacity_allcase %>%
    filter(!(Fuel == 'Adv. Nuclear'),!(Fuel == 'LDS Hydrogen'))
  cap_limit_5p = c(0,4)
  cap_break_5p = seq(from = 0, to = 4, by = 1)
  cap_limit_10p = c(0,8)
  cap_break_10p = seq(from = 0, to = 8, by = 2)
  cap_limit_10p_full = c(0,8)
  cap_limit_10p_ces = c(0,15)
  cap_break_10p_ces = seq(from = 0, to = 16, by = 2)
  cap_limit_25p = c(0,32)
  cap_break_25p = seq(from = 0, to = 32, by = 8)
} else {
  cfe_capacity_allcase = cfe_capacity_allcase %>%
    filter(!(Fuel == 'LDS Hydrogen'))
  cap_limit_5p = c(0,25)
  cap_break_5p = seq(from = 0, to = 25, by = 5)
  cap_limit_10p = c(0,45)
  cap_break_10p = seq(from = 0, to = 45, by = 10)
  cap_limit_10p_full = c(0,25)
  cap_limit_10p_ces = c(0,45)
  cap_break_10p_ces = seq(from = 0, to = 45, by = 10)
  cap_limit_25p = c(0,135)
  cap_break_25p = seq(from = 0, to = 135, by = 20)
}

ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^10%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('CFE Score')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  coord_cartesian(ylim = cap_limit_10p)+
  scale_y_continuous(breaks = cap_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p_new.png'),
         width = 15,
         height = 5)

ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^10%',Scenario),
                     grepl('CES',Scenario),
                     grepl('Curt.', Scenario),
                     (!grepl('Annual',TechSensitivity)&abs(`Shortfall price`)>1) |((grepl('Annual',TechSensitivity))))) +
  geom_col(aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('CFE Score')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  coord_cartesian(ylim = cap_limit_10p_ces)+
  scale_y_continuous(breaks = cap_break_10p_ces)+
  theme_bw() +
  theme(legend.position = 'none')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p_new_ces_curt.png'),
         width = 5,
         height = 5)
ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^10%',Scenario),
                     grepl('CES',Scenario),
                     grepl('no Comb.', Scenario),
                     (!grepl('Annual',TechSensitivity)&abs(`Shortfall price`)>1) |((grepl('Annual',TechSensitivity))))) +
  geom_col(aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('CFE Score')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  coord_cartesian(ylim = cap_limit_10p_ces)+
  scale_y_continuous(breaks = cap_break_10p_ces)+
  theme_bw() +
  theme(legend.position = 'none')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p_new_ces_nocomb.png'),
         width = 5,
         height = 5)

ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^10%',Scenario),
                     grepl('CES',Scenario),
                     grepl('Full', Scenario),
                     (!grepl('Annual',TechSensitivity)&abs(`Shortfall price`)>1) |((grepl('Annual',TechSensitivity))))) +
  geom_col(aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('CFE Score')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  coord_cartesian(ylim = cap_limit_10p_ces)+
  scale_y_continuous(breaks = cap_break_10p_ces)+
  theme_bw() +
  theme(legend.position = 'none')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p_new_ces_full.png'),
         width = 5,
         height = 5)


ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^10%',Scenario),
                     grepl('Full',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('45Q',Scenario),
                     abs(`Shortfall price`)>1,
                     Target > 0.81))+
  geom_col(aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('CFE Score')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit_10p_full)+
  scale_y_continuous(breaks = cap_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p_adv_full_sensitivity.png'),
         width = 10,
         height = 5)
ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^10%',Scenario),
                     grepl('Full',Scenario),
                     !grepl('CES',Scenario),
                     !grepl('Hi.',Scenario),
                     abs(`Shortfall price`)>1))+
  geom_col(aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 2) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('CFE Score')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = cap_limit_10p_full)+
  scale_y_continuous(breaks = cap_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p_adv_full_sensitivity_45q.png'),
         width = 10,
         height = 5)

ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^10%',Scenario),
                     !grepl('CES',Scenario)))+
  geom_col(aes(x = round(`Target`,2), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('Target CFE')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_limit_10p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = cap_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p_all.png'),
         width = p_width_all,
         height = p_height_all)

ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^10%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x = round(`Target`,2), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('Target CFE')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_limit_10p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = cap_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p.png'),
         width = 4,
         height = 9)
ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^5%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x = round(`Target`,2), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('Target CFE')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_limit_5p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = cap_break_5p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_5p.png'),
         width = 4,
         height = 9)

ggplot(data = filter(cfe_capacity_allcase,
                     grepl('^25%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x = round(`Target`,2), 
               y = Capacity/1000, 
               fill = Fuel),
           colour="black", size= 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('Target CFE')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(xlim = c(0.8,1), ylim = cap_limit_25p)+
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
  scale_y_continuous(breaks = cap_break_25p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_25p.png'),
         width = 4,
         height = 9)
# ggplot(data = filter(cfe_capacity_allcase,
#                      !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
#   geom_col(aes(x = round(`Target`,2), 
#                y = Capacity/1000, 
#                fill = Fuel),
#            colour="black", size= 0.1) +
#   facet_wrap(Scenario~.,scales = 'free_y') +
#   scale_fill_manual(name = "Resources", values = fuel_colors) +
#   xlab('Targeted CFE Score')+
#   ylab('Capacity (GW)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   coord_cartesian(xlim = c(0.8,1))+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_51025p.png'),
#          width = p_width_all,
#          height = p_height_all)


# ggplot(data = filter(cfe_capacity_allcase,
#                      grepl('^10%',Scenario),
#                      grepl('CES',Scenario)))+
#   geom_col(aes(x = round(`Target`,2), 
#                y = Capacity/1000, 
#                fill = Fuel),
#            colour="black", size= 0.1) +
#   facet_wrap(Scenario~.) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) +
#   xlab('Target CFE')+
#   ylab('Capacity (GW)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   coord_cartesian(xlim = c(0.8,1), ylim = cap_limit_10p)+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = cap_break_10p)+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_10p_ces.png'),
#          width = p_width_all,
#          height = p_height_all)
# ggplot(data = filter(cfe_capacity_allcase,
#                      grepl('^5%',Scenario)))+
#   geom_col(aes(x = round(`Target`,2), 
#                y = Capacity/1000, 
#                fill = Fuel),
#            colour="black", size= 0.1) +
#   facet_wrap(Scenario~.) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) +
#   xlab('Targeted CFE Score')+
#   ylab('Capacity (GW)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   coord_cartesian(xlim = c(0.8,1), ylim = cap_limit_5p)+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = cap_break_5p)+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_5p.png'),
#          width = p_width_all,
#          height = p_height_all)
# 
# ggplot(data = filter(cfe_capacity_allcase,
#                      grepl('^25%',Scenario)))+
#   geom_col(aes(x = round(`Target`,2), 
#                y = Capacity/1000, 
#                fill = Fuel),
#            colour="black", size= 0.1) +
#   facet_wrap(Scenario~.) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) +
#   xlab('Targeted CFE Score')+
#   ylab('Capacity (GW)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   coord_cartesian(xlim = c(0.8,1), ylim = cap_limit_25p)+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = cap_break_25p)+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_25p.png'),
#          width = p_width_all,
#          height = p_height_all)

cfe_capacity_allcase_annual100 <- read_csv(paste0(temp_RunFdr,"/CompiledResults/",subreg,
                                                  "/Generation/CFE_Gen_Capacity_",subreg,".csv")) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  filter(TechSensitivity == 'Annual 100%')


if (Studyregion == 'WECC') {
  cfe_capacity_allcase_annual100 = cfe_capacity_allcase_annual100 %>%
    filter(!(Fuel == 'Adv. Nuclear'))
}
ggplot(data = filter(cfe_capacity_allcase_annual100, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x= TechSensitivity, 
               y = Capacity/1000, 
               fill=Fuel),
           colour="black", size= 0.1, width = 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  coord_cartesian(ylim = cap_limit_10p)+
  scale_y_continuous(breaks = cap_break_10p)+
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_annual100_10p.png'),
         width = 4,
         height = 9)
ggplot(data = filter(cfe_capacity_allcase_annual100, 
                     grepl('^5%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x= TechSensitivity, 
               y = Capacity/1000, 
               fill=Fuel),
           colour="black", size= 0.1, width = 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  coord_cartesian(ylim = cap_limit_5p)+
  scale_y_continuous(breaks = cap_break_5p)+
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_annual100_5p.png'),
         width = 4,
         height = 9)

ggplot(data = filter(cfe_capacity_allcase_annual100, 
                     grepl('^25%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x= TechSensitivity, 
               y = Capacity/1000, 
               fill=Fuel),
           colour="black", size= 0.1, width = 0.1) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  xlab('')+
  ylab('Capacity (GW)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  coord_cartesian(ylim = cap_limit_25p)+
  scale_y_continuous(breaks = cap_break_25p)+
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_annual100_25p.png'),
         width = 4,
         height = 9)

# 
# ggplot(data = filter(cfe_capacity_allcase_annual100, 
#                      grepl('^10%',Scenario),
#                      !grepl('CES',Scenario),
#                      grepl('Curt. Tech.',Scenario)))+
#   geom_col(aes(x= TechSensitivity, 
#                y = Capacity/1000, 
#                fill=Fuel),
#            colour="black", size= 0.1, width = 0.1) +
#   facet_wrap(Scenario~.,nrow = 2) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   xlab('')+
#   ylab('Capacity (GW)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   theme_bw() +
#   coord_cartesian(ylim = cap_limit_10p)+
#   scale_y_continuous(breaks = cap_break_10p)+
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_annual100_10p.png'),
#          width = p_width_ref,
#          height = p_height_ref)
# 
# ggplot(data = filter(cfe_capacity_allcase_annual100, 
#                      grepl('^10%',Scenario),
#                      grepl('CES',Scenario),
#                      grepl('Curt. Tech.',Scenario)))+
#   geom_col(aes(x= TechSensitivity, 
#                y = Capacity/1000, 
#                fill=Fuel),
#            colour="black", size= 0.1, width = 0.1) +
#   facet_wrap(Scenario~.,nrow = 2) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   xlab('')+
#   ylab('Capacity (GW)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   theme_bw() +
#   coord_cartesian(ylim = cap_limit_10p)+
#   scale_y_continuous(breaks = cap_break_10p)+
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_annual100_10p_ces.png'),
#          width = p_width_ref,
#          height = p_height_ref)
# 
# ggplot(data = filter(cfe_capacity_allcase_annual100, 
#                      grepl('^5%',Scenario),
#                      !grepl('CES',Scenario),
#                      grepl('Curt. Tech.',Scenario)))+
#   geom_col(aes(x= TechSensitivity, 
#                y = Capacity/1000, 
#                fill=Fuel),
#            colour="black", size= 0.1, width = 0.1) +
#   facet_wrap(Scenario~.,nrow = 2) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   xlab('')+
#   ylab('Capacity (GW)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   theme_bw() +
#   coord_cartesian(ylim = cap_limit_5p)+
#   scale_y_continuous(breaks = cap_break_5p)+
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_annual100_5p.png'),
#          width = p_width_ref,
#          height = p_height_ref)
# 
# ggplot(data = filter(cfe_capacity_allcase_annual100, 
#                      grepl('^25%',Scenario),
#                      !grepl('CES',Scenario),
#                      grepl('Curt. Tech.',Scenario)))+
#   geom_col(aes(x= TechSensitivity, 
#                y = Capacity/1000, 
#                fill=Fuel),
#            colour="black", size= 0.1, width = 0.1) +
#   facet_wrap(Scenario~.,nrow = 2) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   xlab('')+
#   ylab('Capacity (GW)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   theme_bw() +
#   coord_cartesian(ylim = cap_limit_25p)+
#   scale_y_continuous(breaks = cap_break_25p)+
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Capacity_Expansion_annual100_25p.png'),
#          width = p_width_ref,
#          height = p_height_ref)

