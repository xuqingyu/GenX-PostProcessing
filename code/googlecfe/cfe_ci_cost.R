# C&I LSE cost ----
cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))
ci_lsecost_total <- read_csv(paste0(temp_RunFdr,'/CompiledResults/ci_lse_cost.csv')) %>%
  group_by(case, year, Scenario, TechSensitivity) %>%
  summarize(value_permwh = sum(value_permwh)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list))%>%
  left_join(select(cfe_score, Scenario, TechSensitivity, `Post-Grid CFE Score Local_n_Import`,`Target`,`Shortfall price`)) 

No24rows = which(ci_lsecost_total$TechSensitivity == 'No 24x7 Purchase')
ci_lsecost_total$Target[No24rows] <- ci_lsecost_total$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(ci_lsecost_total$TechSensitivity == 'Annual 100%')
ci_lsecost_total$Target[Annualrows] <- ci_lsecost_total$`Post-Grid CFE Score Local_n_Import`[Annualrows]
ci_lsecost_total <- ci_lsecost_total %>%
  arrange(Target)
write_csv(ci_lsecost_total, paste0(temp_RunFdr,'/CompiledResults/ci_lse_cost_total.csv'))
modifiedrow = which((ci_lsecost_total$TechSensitivity=='Annual 100%')&
                      (ci_lsecost_total$Scenario=='10% CI Part., Curt. Tech., 80% CES'))
ci_lsecost_total$Target[modifiedrow] = ci_lsecost_total$Target[modifiedrow] - 0.01
modifiedrow = which((ci_lsecost_total$TechSensitivity=='Annual 100%')&
                      (ci_lsecost_total$Scenario=='10% CI Part., Adv. Tech. Full, 80% CES'))
ci_lsecost_total$Target[modifiedrow] = ci_lsecost_total$Target[modifiedrow] - 0.01
modifiedrow = which((ci_lsecost_total$TechSensitivity=='Annual 100%')&
                      (ci_lsecost_total$Scenario=='10% CI Part., Adv. Tech. no Comb., 80% CES'))
ci_lsecost_total$Target[modifiedrow] = ci_lsecost_total$Target[modifiedrow] - 0.01


ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^10%',Scenario),
                         !grepl('CES',Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^10%',Scenario),
                           !grepl('CES',Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'none') +
  xlab('CFE Score') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(ylim = c(0,110)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_10p_new.png'),
         width = 6,
         height = 6)

ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^5%',Scenario),
                         !grepl('CES',Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^5%',Scenario),
                           !grepl('CES',Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'none') +
  xlab('CFE Score') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(ylim = c(0,110)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_5p_new.png'),
         width = 6,
         height = 6)

ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^25%',Scenario),
                         !grepl('CES',Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^25%',Scenario),
                           !grepl('CES',Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'none') +
  xlab('CFE Score') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(ylim = c(0,110)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_25p_new.png'),
         width = 6,
         height = 6)


ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^10%',Scenario),
                         !grepl('CES',Scenario),
                         grepl('Full',Scenario),
                         !grepl('Hi.', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase'),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^10%',Scenario),
                           !grepl('CES',Scenario),
                           grepl('Full',Scenario),
                           !grepl('Hi.', Scenario), 
                           TechSensitivity == 'No 24x7 Purchase'), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'bottom') +
  xlab('CFE Score') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(ylim = c(0,110)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_10p_new_45Q.png'),
         width = 6,
         height = 6)


ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^10%',Scenario),
                         grepl('CES',Scenario),
                         grepl('Curt.', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase',
                         ((TechSensitivity == 'Annual 100%')|((TechSensitivity != 'Annual 100%') && (abs(`Shortfall price`)>1) ) )),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^10%',Scenario),
                           grepl('CES',Scenario),
                           grepl('Curt.', Scenario), 
                           TechSensitivity == 'No 24x7 Purchase'), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'none') +
  xlab('CFE Score') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(ylim = c(0,110)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_10p_ces_curt_new.png'),
         width = 6,
         height = 6)

ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^10%',Scenario),
                         grepl('CES',Scenario),
                         grepl('Full', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase',
                         ((TechSensitivity == 'Annual 100%')|((TechSensitivity != 'Annual 100%') && (abs(`Shortfall price`)>1) ) )),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^10%',Scenario),
                           grepl('CES',Scenario),
                           grepl('Full', Scenario), 
                           TechSensitivity == 'No 24x7 Purchase'), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'none') +
  xlab('CFE Score') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(ylim = c(0,110)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_10p_ces_full_new.png'),
         width = 6,
         height = 6)
ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^10%',Scenario),
                         grepl('CES',Scenario),
                         grepl('no Comb', Scenario), 
                         TechSensitivity != 'No 24x7 Purchase',
                         ((TechSensitivity == 'Annual 100%')|((TechSensitivity != 'Annual 100%') && (abs(`Shortfall price`)>1) ) )),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^10%',Scenario),
                           grepl('CES',Scenario),
                           grepl('no Comb.', Scenario), 
                           TechSensitivity == 'No 24x7 Purchase'), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'none') +
  xlab('CFE Score') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(ylim = c(0,110)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_10p_ces_nocomb_new.png'),
         width = 6,
         height = 6)


ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^10%',Scenario),
                         !grepl('CES',Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario), 
                         abs(`Shortfall price`)>0),
           aes(x = round(`Target`,2), 
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3,
           width = 0.01) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^10%',Scenario),
                           !grepl('CES',Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = T) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  theme(legend.position = 'bottom') +
  xlab('Target CFE') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(xlim = c(0.8,1), ylim = c(0,110)) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_10p.png'),
         width = 6,
         height = 6)
ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^5%',Scenario),
                         !grepl('CES',Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario), 
                         abs(`Shortfall price`)>0),
           aes(x = round(`Target`,2), 
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3,
           width = 0.01) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^5%',Scenario),
                           !grepl('CES',Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = T) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  theme(legend.position = 'bottom') +
  xlab('Target CFE') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(xlim = c(0.8,1), ylim = c(0,110)) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_5p.png'),
         width = 6,
         height = 6)

ggplot() +
  geom_col(data = filter(ci_lsecost_total, 
                         grepl('^25%',Scenario),
                         !grepl('CES',Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario), 
                         abs(`Shortfall price`)>0),
           aes(x = round(`Target`,2), 
               y = value_permwh, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3,
           width = 0.01) +
  theme_bw() +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference, 
                           grepl('^25%',Scenario),
                           !grepl('CES',Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),), 
             aes(yintercept = ci_lsecost/`Participated Load`, 
                 color = TechSensitivity),
             show.legend = T) +
  scale_color_brewer(palette = 'Set2') +
  geom_hline(yintercept = 0, color = 'black')+
  theme(legend.position = 'bottom') +
  xlab('Target CFE') +
  ylab('24x7 C&I Cost ($/MWh)') +
  coord_cartesian(xlim = c(0.8,1), ylim = c(0,110)) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_25p.png'),
         width = 6,
         height = 6)

# ggplot() +
#   geom_col(data = filter(ci_lsecost_total, 
#                          grepl('^10%',Scenario),
#                          grepl('CES',Scenario), 
#                          abs(`Shortfall price`)>0),
#            aes(x = round(`Target`,2), 
#                y = value_permwh, 
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   theme_bw() +
#   scale_fill_brewer(palette = 'Set1') +
#   geom_hline(data = filter(reference, 
#                            grepl('^10%',Scenario),
#                            grepl('CES',Scenario)), 
#              aes(yintercept = ci_lsecost/`Participated Load`, 
#                  color = TechSensitivity),
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   geom_hline(yintercept = 0, color = 'black')+
#   theme(legend.position = 'bottom') +
#   xlab('Targeted CFE') +
#   ylab('24x7 C&I Cost ($/MWh)') +
#   coord_cartesian(xlim = c(0.8,1), ylim = c(0,100)) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_10p_ces.png'),
#          width = 7,
#          height = 7)
# 
# 
# ggplot() +
#   geom_col(data = filter(ci_lsecost_total, 
#                          grepl('^5%',Scenario),
#                          !grepl('CES',Scenario), 
#                          abs(`Shortfall price`)>0),
#            aes(x = round(`Target`,2), 
#                y = value_permwh, 
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   theme_bw() +
#   scale_fill_brewer(palette = 'Set1') +
#   geom_hline(data = filter(reference, 
#                            grepl('^5%',Scenario),
#                            !grepl('CES',Scenario)), 
#              aes(yintercept = ci_lsecost/`Participated Load`, 
#                  color = TechSensitivity),
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   geom_hline(yintercept = 0, color = 'black')+
#   theme(legend.position = 'bottom') +
#   xlab('Targeted CFE') +
#   ylab('24x7 C&I Cost ($/MWh)') +
#   coord_cartesian(xlim = c(0.8,1), ylim = c(0,100)) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_5p.png'),
#          width = 7,
#          height = 7)
# 
# ggplot() +
#   geom_col(data = filter(ci_lsecost_total, 
#                          grepl('^25%',Scenario),
#                          !grepl('CES',Scenario), 
#                          abs(`Shortfall price`)>0),
#            aes(x = round(`Target`,2), 
#                y = value_permwh, 
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   theme_bw() +
#   scale_fill_brewer(palette = 'Set1') +
#   geom_hline(data = filter(reference, 
#                            grepl('^25%',Scenario),
#                            !grepl('CES',Scenario)), 
#              aes(yintercept = ci_lsecost/`Participated Load`, 
#                  color = TechSensitivity),
#              show.legend = T) +
#   scale_color_brewer(palette = 'Set2') +
#   geom_hline(yintercept = 0, color = 'black')+
#   theme(legend.position = 'bottom') +
#   xlab('Targeted CFE') +
#   ylab('24x7 C&I Cost ($/MWh)') +
#   coord_cartesian(xlim = c(0.8,1), ylim = c(0,100)) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10)) +
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CILSECost_25p.png'),
#          width = 7,
#          height = 7)
