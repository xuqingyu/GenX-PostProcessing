# System emissions ----
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

modifiedrow = which((cfe_emission_table_withname$TechSensitivity=='Annual 100%')&
                      (cfe_emission_table_withname$Scenario=='10% CI Part., Curt. Tech., 80% CES'))
cfe_emission_table_withname$Target[modifiedrow] = cfe_emission_table_withname$Target[modifiedrow] - 0.01
modifiedrow = which((cfe_emission_table_withname$TechSensitivity=='Annual 100%')&
                      (cfe_emission_table_withname$Scenario=='10% CI Part., Adv. Tech. Full, 80% CES'))
cfe_emission_table_withname$Target[modifiedrow] = cfe_emission_table_withname$Target[modifiedrow] - 0.01
modifiedrow = which((cfe_emission_table_withname$TechSensitivity=='Annual 100%')&
                      (cfe_emission_table_withname$Scenario=='10% CI Part., Adv. Tech. no Comb., 80% CES'))
cfe_emission_table_withname$Target[modifiedrow] = cfe_emission_table_withname$Target[modifiedrow] - 0.01


ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^10%',Scenario),
                         grepl('CES',Scenario),
                         grepl('Curt.', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase',
                         ((TechSensitivity == 'Annual 100%')|((TechSensitivity != 'Annual 100%') & (abs(`Shortfall price`)>1) ) )),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = (as.numeric(`emission_local_n_import`))/1e6,
               fill = Scenario),
           color = 'black',
           size = 0.3,
           position = 'dodge')  +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  geom_hline(data = filter(reference,
                           grepl('^10%',Scenario),
                           grepl('CES',Scenario),
                           grepl('Curt.', Scenario), 
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = (as.numeric(`system_emission`))/1e6,
                 color = TechSensitivity),
             show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian( ylim = emission_limit_ces) +
  scale_y_continuous(breaks = emission_break)+
  geom_hline(yintercept = 0, size = 0.3)+
  ylab('Emission (Mtons)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') +
  guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_10p_ces_curt_new.png'),
         width = 6,
         height = 6)

ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^10%',Scenario),
                         grepl('CES',Scenario),
                         grepl('Full', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase',
                         ((TechSensitivity == 'Annual 100%')|((TechSensitivity != 'Annual 100%') & (abs(`Shortfall price`)>1) ) )),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = (as.numeric(`emission_local_n_import`))/1e6,
               fill = Scenario),
           color = 'black',
           size = 0.3,
           position = 'dodge')  +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  geom_hline(data = filter(reference,
                           grepl('^10%',Scenario),
                           grepl('CES',Scenario),
                           grepl('Full', Scenario), 
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = (as.numeric(`system_emission`))/1e6,
                 color = TechSensitivity),
             show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  geom_hline(yintercept = 0, size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian( ylim = emission_limit_ces) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission (Mtons)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') +
  guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_10p_ces_full_new.png'),
         width = 6,
         height = 6)
ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^10%',Scenario),
                         grepl('CES',Scenario),
                         grepl('no Comb.', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase',
                         ((TechSensitivity == 'Annual 100%')|((TechSensitivity != 'Annual 100%') & (abs(`Shortfall price`)>1) ) )),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = (as.numeric(`emission_local_n_import`))/1e6,
               fill = Scenario),
           color = 'black',
           size = 0.3,
           position = 'dodge')  +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  geom_hline(data = filter(reference,
                           grepl('^10%',Scenario),
                           grepl('CES',Scenario),
                           grepl('no Comb', Scenario), 
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = (as.numeric(`system_emission`))/1e6,
                 color = TechSensitivity),
             show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  geom_hline(yintercept = 0, size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian( ylim = emission_limit_ces) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission (Mtons)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') +
  guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_10p_ces_nocomb_new.png'),
         width = 6,
         height = 6)


ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^10%',Scenario),
                         !grepl('CES|Hi.|45Q|Ex.', Scenario),
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = (as.numeric(`emission_local_n_import`))/1e6,
               fill = Scenario),
           color = 'black',
           size = 0.3,
           position = 'dodge')  +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  geom_hline(data = filter(reference,
                           grepl('^10%',Scenario),
                           !grepl('CES|Hi.|45Q|Ex.', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = (as.numeric(`system_emission`))/1e6,
                 color = TechSensitivity),
             show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian( ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission (Mtons)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') +
  guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_10p_new.png'),
         width = 6,
         height = 6)

ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^5%',Scenario),
                         !grepl('CES|Hi.|45Q|Ex.', Scenario),
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = (as.numeric(`emission_local_n_import`))/1e6,
               fill = Scenario),
           color = 'black',
           size = 0.3,
           position = 'dodge')  +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  geom_hline(data = filter(reference,
                           grepl('^5%',Scenario),
                           !grepl('CES|Hi.|45Q|Ex.', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = (as.numeric(`system_emission`))/1e6,
                 color = TechSensitivity),
             show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian( ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission (Mtons)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') +
  guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_5p_new.png'),
         width = 6,
         height = 6)

ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^25%',Scenario),
                         !grepl('CES|Hi.|45Q|Ex.', Scenario),
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = (as.numeric(`emission_local_n_import`))/1e6,
               fill = Scenario),
           color = 'black',
           size = 0.3,
           position = 'dodge')  +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  geom_hline(data = filter(reference,
                           grepl('^25%',Scenario),
                           !grepl('CES|Hi.|45Q|Ex.', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = (as.numeric(`system_emission`))/1e6,
                 color = TechSensitivity),
             show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian( ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission (Mtons)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') +
  guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_25p_new.png'),
         width = 6,
         height = 6)

# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^10%',Scenario),
#                          !grepl('CES|Hi.|45Q|Ex.', Scenario),
#                          abs(`Shortfall price`)>0),
#            aes(x = round(`Target`,2), 
#                y = (as.numeric(`emission_local_n_import`))/1e6,
#                fill = Scenario),
#            color = 'black',
#            size = 0.1,
#            position = 'dodge',
#            width = 0.01)  +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() +
#   geom_hline(data = filter(reference,
#                            grepl('^10%',Scenario),
#                            !grepl('CES|Hi.|45Q|Ex.', Scenario)), 
#              aes(yintercept = (as.numeric(`system_emission`))/1e6,
#                  color = TechSensitivity), 
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission (Mtons)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') +
#   guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_10p.png'),
#          width = 6,
#          height = 6)

# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^10%',Scenario),
#                          grepl('CES', Scenario),
#                          abs(`Shortfall price`)>0),
#            aes(x = round(`Target`,2), 
#                y = (as.numeric(`emission_local_n_import`))/1e6,
#                fill = Scenario),
#            color = 'black',
#            size = 0.1,
#            position = 'dodge',
#            width = 0.01)  +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() +
#   geom_hline(data = filter(reference,
#                            grepl('^10%',Scenario),
#                            grepl('CES', Scenario)), 
#              aes(yintercept = (as.numeric(`system_emission`))/1e6,
#                  color = TechSensitivity), 
#              show.legend = T) +
#   geom_hline(yintercept = 0, color = 'gray30')+
#   scale_color_brewer(palette = 'Set2') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit_ces) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission (Mtons)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') +
#   guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_10p_ces.png'),
#          width = 7,
#          height = 7)

# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^5%',Scenario),
#                          !grepl('CES|Hi.|45Q|Ex.', Scenario),
#                          abs(`Shortfall price`)>0),
#            aes(x = round(`Target`,2), 
#                y = (as.numeric(`emission_local_n_import`))/1e6,
#                fill = Scenario),
#            color = 'black',
#            size = 0.1,
#            position = 'dodge',
#            width = 0.01)  +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() +
#   geom_hline(data = filter(reference,
#                            grepl('^5%',Scenario),
#                            !grepl('CES|Hi.|45Q|Ex.', Scenario)), 
#              aes(yintercept = (as.numeric(`system_emission`))/1e6,
#                  color = TechSensitivity), 
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission (Mtons)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') +
#   guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_5p.png'),
#          width = 6,
#          height = 6)
# 
# 
# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^25%',Scenario),
#                          !grepl('CES|Hi.|45Q|Ex.', Scenario),
#                          abs(`Shortfall price`)>0),
#            aes(x = round(`Target`,2), 
#                y = (as.numeric(`emission_local_n_import`))/1e6,
#                fill = Scenario),
#            color = 'black',
#            size = 0.1,
#            position = 'dodge',
#            width = 0.01)  +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() +
#   geom_hline(data = filter(reference,
#                            grepl('^25%',Scenario),
#                            !grepl('CES|Hi.|45Q|Ex.', Scenario)), 
#              aes(yintercept = (as.numeric(`system_emission`))/1e6,
#                  color = TechSensitivity), 
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission (Mtons)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') +
#   guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Emission_comparison_25p.png'),
#          width = 6,
#          height = 6)
