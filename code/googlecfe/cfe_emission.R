# C&I emissions ----
cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))
cfe_emission_table_withname = read_csv(paste0(temp_RunFdr,'/CompiledResults/ci_emissions.csv'), 
                                       col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list), 
         Scenario = factor(Scenario, levels = y_list)) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity'))%>%
  mutate(emission_permwh = emission_measure/as.numeric(`Participated Load`))
  

if (Studyregion == 'WECC') {
  emission_limit = c(0,0.2)
  emission_break = seq(0,0.2, 0.02)
} else {
  emission_limit = c(0,0.3)
  emission_break = seq(0,0.3, 0.02)
}

No24rows = which(cfe_emission_table_withname$TechSensitivity == 'No 24x7 Purchase')
cfe_emission_table_withname$Target[No24rows] <- cfe_emission_table_withname$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_emission_table_withname$TechSensitivity == 'Annual 100%')
cfe_emission_table_withname$Target[Annualrows] <- cfe_emission_table_withname$`Post-Grid CFE Score Local_n_Import`[Annualrows]

cfe_emission_table_withname <- cfe_emission_table_withname %>%
  arrange(Target)

ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^10%',Scenario),
                         !grepl('CES', Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = emission_measure/as.numeric(`Participated Load`),
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() + 
  geom_hline(data = filter(reference,
                           grepl('^10%',Scenario),
                           !grepl('CES', Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'
  ),
  aes(yintercept = ci_emission_system/as.numeric(`Participated Load`),
      color = TechSensitivity),
  show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black') +
  coord_cartesian(ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission Rate (ton/MWh)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_10p_new.png'),
         width = 6,
         height = 6)
ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^5%',Scenario),
                         !grepl('CES', Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = emission_measure/as.numeric(`Participated Load`),
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() + 
  geom_hline(data = filter(reference,
                           grepl('^5%',Scenario),
                           !grepl('CES', Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'
  ),
  aes(yintercept = ci_emission_system/as.numeric(`Participated Load`),
      color = TechSensitivity),
  show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black') +
  coord_cartesian(ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission Rate (ton/MWh)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_5p_new.png'),
         width = 6,
         height = 6)
ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^25%',Scenario),
                         !grepl('CES', Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = emission_measure/as.numeric(`Participated Load`),
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() + 
  geom_hline(data = filter(reference,
                           grepl('^25%',Scenario),
                           !grepl('CES', Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'
  ),
  aes(yintercept = ci_emission_system/as.numeric(`Participated Load`),
      color = TechSensitivity),
  show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black') +
  coord_cartesian(ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission Rate (ton/MWh)') +
  xlab('CFE Score') +
  theme(legend.position = 'none') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_25p_new.png'),
         width = 6,
         height = 6)
# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^10%',Scenario),
#                          !grepl('CES', Scenario),
#                          # !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
#                          !grepl('Hi. Nat. Gas P.', Scenario),
#                          abs(`Shortfall price`)>0), 
#            aes(x = round(Target,2), 
#                y = emission_measure/as.numeric(`Participated Load`),
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() + 
#   geom_hline(data = filter(reference,
#                            grepl('^10%',Scenario),
#                            !grepl('CES', Scenario),
#                            # !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
#                            !grepl('Hi. Nat. Gas P.', Scenario),
#                            ), 
#              aes(yintercept = ci_emission_system/as.numeric(`Participated Load`), 
#                  color = TechSensitivity), 
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   geom_hline(yintercept = 0, color = 'black') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission Rate (ton/MWh)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_10p_45q.png'),
#          width = 7,
#          height = 7)
# 
# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^10%',Scenario),
#                          !grepl('CES', Scenario),
#                          !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
#                          abs(`Shortfall price`)>0), 
#            aes(x = round(Target,2), 
#                y = emission_measure/as.numeric(`Participated Load`),
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() + 
#   geom_hline(data = filter(reference,
#                            grepl('^10%',Scenario),
#                            !grepl('CES', Scenario),
#                            !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
#   ), 
#   aes(yintercept = ci_emission_system/as.numeric(`Participated Load`), 
#       color = TechSensitivity), 
#   show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   geom_hline(yintercept = 0, color = 'black') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission Rate (ton/MWh)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_10p.png'),
#          width = 6,
#          height = 6)


# Note if adv. tech is open to general public, 
# they will be selected in the reference case and annual matching case,
# leading to several horizontal lines

# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^10%',Scenario),
#                          grepl('CES', Scenario),
#                          abs(`Shortfall price`)>0), 
#            aes(x = round(Target,2), 
#                y = emission_measure/as.numeric(`Participated Load`),
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() + 
#   geom_hline(data = filter(reference,
#                            grepl('^10%',Scenario),
#                            grepl('CES', Scenario),), 
#              aes(yintercept = ci_emission_system/as.numeric(`Participated Load`), 
#                  color = TechSensitivity), 
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   geom_hline(yintercept = 0, color = 'black') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission Rate (ton/MWh)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_10p_ces.png'),
#          width = 7,
#          height = 7)

# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^5%',Scenario),
#                          !grepl('CES', Scenario),
#                          abs(`Shortfall price`)>0), 
#            aes(x = round(Target,2), 
#                y = emission_measure/as.numeric(`Participated Load`),
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() + 
#   geom_hline(data = filter(reference,
#                            grepl('^5%',Scenario),
#                            !grepl('CES', Scenario),), 
#              aes(yintercept = ci_emission_system/as.numeric(`Participated Load`), 
#                  color = TechSensitivity), 
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   geom_hline(yintercept = 0, color = 'black') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission Rate (ton/MWh)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_5p.png'),
#          width = 6,
#          height = 6)
# 
# ggplot()+
#   geom_col(data = filter(cfe_emission_table_withname,
#                          grepl('^25%',Scenario),
#                          !grepl('CES', Scenario),
#                          abs(`Shortfall price`)>0), 
#            aes(x = round(Target,2), 
#                y = emission_measure/as.numeric(`Participated Load`),
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   scale_fill_brewer(palette = 'Set1') +
#   theme_bw() + 
#   geom_hline(data = filter(reference,
#                            grepl('^25%',Scenario),
#                            !grepl('CES', Scenario),), 
#              aes(yintercept = ci_emission_system/as.numeric(`Participated Load`), 
#                  color = TechSensitivity), 
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   geom_hline(yintercept = 0, color = 'black') +
#   coord_cartesian(xlim = c(0.8,1), ylim = emission_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02))+
#   scale_y_continuous(breaks = emission_break)+
#   ylab('Emission Rate (ton/MWh)') +
#   xlab('Target CFE') +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_25p.png'),
#          width = 6,
#          height = 6)
