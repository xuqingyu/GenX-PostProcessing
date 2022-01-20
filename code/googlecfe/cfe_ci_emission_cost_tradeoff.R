

# Emission Cost trade off curve:
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

ci_lsecost_total <- read_csv(paste0(temp_RunFdr,'/CompiledResults/ci_lse_cost.csv')) %>%
  group_by(case, year, Scenario, TechSensitivity) %>%
  summarize(value_permwh = sum(value_permwh)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list))%>%
  left_join(select(cfe_score, Scenario, TechSensitivity, `Post-Grid CFE Score Local_n_Import`,`Target`,`Shortfall price`))


ci_lsecost_emission = left_join(ci_lsecost_total, cfe_emission_table_withname)

No24rows = which(ci_lsecost_emission$TechSensitivity == 'No 24x7 Purchase')
ci_lsecost_emission$Target[No24rows] <- ci_lsecost_emission$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(ci_lsecost_emission$TechSensitivity == 'Annual 100%')
ci_lsecost_emission$Target[Annualrows] <- ci_lsecost_emission$`Post-Grid CFE Score Local_n_Import`[Annualrows]
ci_lsecost_emission <- ci_lsecost_emission %>%
  arrange(Target)

if (Studyregion == 'WECC') {
  emission_limit = c(0.2,0)
  emission_break = seq(0.2, 0, -0.02)
  cost_per_ton_limit = c(-10,260)
  cost_per_ton_break = seq(-40,300, 40)
  cost_per_ton_limit_ces = c(-100,900)
  cost_per_ton_break_ces = seq(-100,900, 100)
} else {
  emission_limit = c(0.3,0)
  emission_break = seq(0.3,0, -0.02)
  cost_per_ton_limit = c(-10,230)
  cost_per_ton_break = seq(-20,240, 20)
  cost_per_ton_limit_ces = c(-100,900)
  cost_per_ton_break_ces = seq(-100,900, 100)
}



ci_lsecost_emission_temp = filter(ci_lsecost_emission, 
                                  !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                  grepl('^10%',Scenario))
ci_lsecost_emission_temp_bm = filter(reference, 
                                     !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                     grepl('^10%',Scenario)) %>%
  select(ci_emission_system, ci_lsecost, `Participated Load`,Scenario,TechSensitivity) %>%
  mutate(value_permwh = ci_lsecost/`Participated Load`,
         emission_permwh = ci_emission_system/`Participated Load`)
base = filter(reference,
              !grepl('CES',Scenario), 
              grepl('^10%',Scenario),
              TechSensitivity == 'No 24x7 Purchase') %>%
  mutate(value_permwh = ci_lsecost/`Participated Load`,
         emission_permwh = ci_emission_system/`Participated Load`) %>%
  rename(value_permwh_base = value_permwh, emission_permwh_base = emission_permwh) %>%
  select(Scenario,  value_permwh_base, emission_permwh_base)
cost_per_ton =  ci_lsecost_emission_temp %>%
  ungroup() %>%
  left_join(base) %>%
  mutate(cost_per_ton = (value_permwh - value_permwh_base)/(emission_permwh_base-emission_permwh)) %>%
  mutate(cost_per_ton = round(cost_per_ton,2)) %>%
  select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`) %>%
  filter(!(grepl('No 24x7 Purchase',TechSensitivity))) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))
#         !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))

ggplot() +
  geom_point(data = filter(ci_lsecost_emission_temp,
                           abs(`Shortfall price`) >0), 
             aes(x = Target,
                 y = value_permwh, 
                 color = Scenario,
                 shape = Scenario)) +
  geom_point(data = filter(ci_lsecost_emission_temp, 
                           !grepl('CES',Scenario), 
                           grepl('^10%',Scenario),
                           TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'),
                           !grepl('Hi. Nat. Gas P.',Scenario)
  ), 
  aes(x = Target,
      y = value_permwh, 
      color = TechSensitivity)) +
  scale_color_brewer(name = 'Scenario', palette = 'Set1')+
  scale_shape_discrete(name = 'Scenario')+
  xlab('CFE Score') +
  ylab('24x7 C&I Cost ($/MWh)') +
  theme_bw() +
  coord_cartesian(ylim = c(0,110)) +
  scale_x_continuous(breaks = seq(0.2, 1, by = 0.05))+
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10))+
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=1), shape=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_Cost_cfe_Trade-off_10p.png'),
         width = 6,
         height = 6)
ggplot() +
  geom_point(data = filter(ci_lsecost_emission_temp,
                           abs(`Shortfall price`) >0), 
             aes(x = emission_permwh,
                 y = value_permwh, 
                 color = Scenario)) +
  geom_point(data = filter(ci_lsecost_emission_temp_bm, 
                           !grepl('CES',Scenario), 
                           grepl('^10%',Scenario),
                           TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'),
                           !grepl('Hi. Nat. Gas P.',Scenario)
                           ), 
             aes(x = emission_permwh,
                 y = value_permwh, 
                 color = TechSensitivity)) +
  scale_color_brewer(palette = 'Set1')+
  xlab('Emission Rate (ton/MWh)') +
  ylab('24x7 C&I Cost ($/MWh)') +
  theme_bw() +
  coord_cartesian(xlim = emission_limit,ylim = c(0,110)) +
  scale_x_reverse(breaks = emission_break)+
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10))+
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_Cost_emission_Trade-off_10p.png'),
         width = 6,
         height = 6)

ggplot() +
  geom_col(data = filter(cost_per_ton), 
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = cost_per_ton, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  geom_hline(yintercept = 0, color = 'gray30')+
  scale_color_brewer(palette = 'Set2')+
  scale_fill_brewer(palette = 'Set1') +
  xlab('CFE Score') +
  ylab('Average Emission Abatement Cost ($/ton)') +
  theme_bw() +
  coord_cartesian(ylim = cost_per_ton_limit) +
  scale_y_continuous(breaks = cost_per_ton_break) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'none') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_abatementcost_10p_new.png'),
         width = 6,
         height = 6)


ggplot() +
  geom_col(data = filter(cost_per_ton,
                         abs(`Shortfall price`) >0), 
           aes(x = round(`Target`,2),
               y = cost_per_ton, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3,
           width = 0.01) +
  geom_hline(data = filter(cost_per_ton,
                           grepl('Annual 100%', TechSensitivity)), 
             aes(yintercept = cost_per_ton, 
                 color = TechSensitivity)) +
  geom_hline(yintercept = 0, color = 'gray30')+
  scale_color_brewer(palette = 'Set2')+
  scale_fill_brewer(palette = 'Set1') +
  xlab('Target CFE') +
  ylab('Average Emission Abatement Cost ($/ton)') +
  theme_bw() +
  coord_cartesian(xlim = c(0.8,1),ylim = cost_per_ton_limit) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = cost_per_ton_break) +
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_abatementcost_10p.png'),
         width = 6,
         height = 6)

ci_lsecost_emission_temp = filter(ci_lsecost_emission, 
                                  !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                  grepl('^5%',Scenario))
ci_lsecost_emission_temp_bm = filter(reference, 
                                     !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                     grepl('^5%',Scenario)) %>%
  select(ci_emission_system, ci_lsecost, `Participated Load`,Scenario,TechSensitivity) %>%
  mutate(value_permwh = ci_lsecost/`Participated Load`,
         emission_permwh = ci_emission_system/`Participated Load`)
base = filter(reference,
              !grepl('CES',Scenario), 
              grepl('^5%',Scenario),
              TechSensitivity == 'No 24x7 Purchase') %>%
  mutate(value_permwh = ci_lsecost/`Participated Load`,
         emission_permwh = ci_emission_system/`Participated Load`) %>%
  rename(value_permwh_base = value_permwh, emission_permwh_base = emission_permwh) %>%
  select(Scenario,  value_permwh_base, emission_permwh_base)
cost_per_ton =  ci_lsecost_emission_temp %>%
  ungroup() %>%
  left_join(base) %>%
  mutate(cost_per_ton = (value_permwh - value_permwh_base)/(emission_permwh_base-emission_permwh)) %>%
  mutate(cost_per_ton = round(cost_per_ton,2)) %>%
  select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`) %>%
  filter(!(grepl('No 24x7 Purchase',TechSensitivity))) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))
#         !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))
ggplot() +
  geom_point(data = filter(ci_lsecost_emission_temp,
                           abs(`Shortfall price`) >0), 
             aes(x = emission_permwh,
                 y = value_permwh, 
                 color = Scenario)) +
  geom_point(data = filter(ci_lsecost_emission_temp_bm, 
                           !grepl('CES',Scenario), 
                           grepl('^5%',Scenario),
                           TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'),
                           !grepl('Hi. Nat. Gas P.',Scenario)
  ), 
  aes(x = emission_permwh,
      y = value_permwh, 
      color = TechSensitivity)) +
  scale_color_brewer(palette = 'Set1')+
  xlab('Emission Rate (ton/MWh)') +
  ylab('24x7 C&I Cost ($/MWh)') +
  theme_bw() +
  coord_cartesian(xlim = emission_limit,ylim = c(0,110)) +
  scale_x_reverse(breaks = emission_break)+
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10))+
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_Cost_emission_Trade-off_5p.png'),
         width = 6,
         height = 6)

ggplot() +
  geom_col(data = filter(cost_per_ton,
                         abs(`Shortfall price`) >0), 
           aes(x = round(`Target`,2),
               y = cost_per_ton, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3,
           width = 0.01) +
  geom_hline(data = filter(cost_per_ton,
                           grepl('Annual 100%', TechSensitivity)), 
             aes(yintercept = cost_per_ton, 
                 color = TechSensitivity)) +
  geom_hline(yintercept = 0, color = 'gray30')+
  scale_color_brewer(palette = 'Set2')+
  scale_fill_brewer(palette = 'Set1') +
  xlab('Target CFE') +
  ylab('Average Emission Abatement Cost ($/ton)') +
  theme_bw() +
  coord_cartesian(xlim = c(0.8,1),ylim = cost_per_ton_limit) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = cost_per_ton_break) +
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_abatementcost_5p.png'),
         width = 6,
         height = 6)

ci_lsecost_emission_temp = filter(ci_lsecost_emission, 
                                  !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                  grepl('^25%',Scenario))
ci_lsecost_emission_temp_bm = filter(reference, 
                                     !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                     grepl('^25%',Scenario)) %>%
  select(ci_emission_system, ci_lsecost, `Participated Load`,Scenario,TechSensitivity) %>%
  mutate(value_permwh = ci_lsecost/`Participated Load`,
         emission_permwh = ci_emission_system/`Participated Load`)
base = filter(reference,
              !grepl('CES',Scenario), 
              grepl('^25%',Scenario),
              TechSensitivity == 'No 24x7 Purchase') %>%
  mutate(value_permwh = ci_lsecost/`Participated Load`,
         emission_permwh = ci_emission_system/`Participated Load`) %>%
  rename(value_permwh_base = value_permwh, emission_permwh_base = emission_permwh) %>%
  select(Scenario,  value_permwh_base, emission_permwh_base)
cost_per_ton =  ci_lsecost_emission_temp %>%
  ungroup() %>%
  left_join(base) %>%
  mutate(cost_per_ton = (value_permwh - value_permwh_base)/(emission_permwh_base-emission_permwh)) %>%
  mutate(cost_per_ton = round(cost_per_ton,2)) %>%
  select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`) %>%
  filter(!(grepl('No 24x7 Purchase',TechSensitivity))) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))
#         !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))
ggplot() +
  geom_point(data = filter(ci_lsecost_emission_temp,
                           abs(`Shortfall price`) >0), 
             aes(x = emission_permwh,
                 y = value_permwh, 
                 color = Scenario)) +
  geom_point(data = filter(ci_lsecost_emission_temp_bm, 
                           !grepl('CES',Scenario), 
                           grepl('^25%',Scenario),
                           TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'),
                           !grepl('Hi. Nat. Gas P.',Scenario)
  ), 
  aes(x = emission_permwh,
      y = value_permwh, 
      color = TechSensitivity)) +
  scale_color_brewer(palette = 'Set1')+
  xlab('Emission Rate (ton/MWh)') +
  ylab('24x7 C&I Cost ($/MWh)') +
  theme_bw() +
  coord_cartesian(xlim = emission_limit,ylim = c(0,110)) +
  scale_x_reverse(breaks = emission_break)+
  scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10))+
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_Cost_emission_Trade-off_25p.png'),
         width = 6,
         height = 6)

ggplot() +
  geom_col(data = filter(cost_per_ton,
                         abs(`Shortfall price`) >0), 
           aes(x = round(`Target`,2),
               y = cost_per_ton, 
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3,
           width = 0.01) +
  geom_hline(data = filter(cost_per_ton,
                           grepl('Annual 100%', TechSensitivity)), 
             aes(yintercept = cost_per_ton, 
                 color = TechSensitivity)) +
  geom_hline(yintercept = 0, color = 'gray30')+
  scale_color_brewer(palette = 'Set2')+
  scale_fill_brewer(palette = 'Set1') +
  xlab('Target CFE') +
  ylab('Average Emission Abatement Cost ($/ton)') +
  theme_bw() +
  coord_cartesian(xlim = c(0.8,1),ylim = cost_per_ton_limit) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = cost_per_ton_break) +
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_abatementcost_25p.png'),
         width = 6,
         height = 6)
# ci_lsecost_emission_temp = filter(ci_lsecost_emission, 
#                                   grepl('CES',Scenario), 
#                                   grepl('^10%',Scenario))
# base = filter(reference,
#               grepl('CES',Scenario), 
#               grepl('^10%',Scenario)) %>%
#   mutate(value_permwh = ci_lsecost/`Participated Load`,
#          emission_permwh = ci_emission_system/`Participated Load`) %>%
#   select(TechSensitivity, value_permwh, emission_permwh)
# cost_per_ton =  ci_lsecost_emission_temp %>%
#   ungroup() %>%
#   mutate(cost_per_ton = (value_permwh - base$value_permwh[1])/(base$emission_permwh[1]-emission_permwh)) %>%
#   mutate(cost_per_ton = round(cost_per_ton,2)) %>%
#   select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`) %>%
#   filter(!(grepl('No 24x7 Purchase',TechSensitivity)),
#          !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))
# ggplot() +
#   geom_point(data = filter(ci_lsecost_emission_temp,
#                            abs(`Shortfall price`) >0), 
#              aes(x = emission_permwh,
#                  y = value_permwh, 
#                  color = Scenario)) +
#   geom_point(data = base, 
#              aes(x = emission_permwh,
#                  y = value_permwh, 
#                  color = TechSensitivity)) +
#   scale_color_brewer(palette = 'Set1')+
#   xlab('Emission Rate (ton/MWh)') +
#   ylab('24x7 C&I Cost ($/MWh)') +
#   theme_bw() +
#   coord_cartesian(xlim = emission_limit,ylim = c(0,100)) +
#   scale_x_continuous(breaks = emission_break)+
#   scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10))+
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_Cost_emission_Trade-off_10p_ces.png'),
#          width = 7,
#          height = 7)
# ggplot() +
#   geom_col(data = filter(cost_per_ton,
#                          abs(`Shortfall price`) >0), 
#            aes(x = round(`Target`,2),
#                y = cost_per_ton, 
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   geom_hline(data = filter(cost_per_ton,
#                            grepl('Annual 100%', TechSensitivity)), 
#              aes(yintercept = cost_per_ton, 
#                  color = TechSensitivity)) +
#   geom_hline(yintercept = 0, color = 'gray30')+
#   scale_color_brewer(palette = 'Set2')+
#   scale_fill_brewer(palette = 'Set1') +
#   xlab('Targeted CFE') +
#   ylab('Average Emission Abatement Cost ($/ton)') +
#   theme_bw() +
#   coord_cartesian(xlim = c(0.8,1),ylim = cost_per_ton_limit_ces) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = cost_per_ton_break_ces) +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_abatementcost_10p_ces.png'),
#          width = 7,
#          height = 7)
# 
# 
# ci_lsecost_emission_temp = filter(ci_lsecost_emission, 
#                                   !grepl('CES',Scenario), 
#                                   grepl('^5%',Scenario))
# base = filter(reference,
#               !grepl('CES',Scenario), 
#               grepl('^5%',Scenario)) %>%
#   mutate(value_permwh = ci_lsecost/`Participated Load`,
#          emission_permwh = ci_emission_system/`Participated Load`) %>%
#   select(TechSensitivity, value_permwh, emission_permwh)
# cost_per_ton =  ci_lsecost_emission_temp %>%
#   ungroup() %>%
#   mutate(cost_per_ton = (value_permwh - base$value_permwh[1])/(base$emission_permwh[1]-emission_permwh)) %>%
#   mutate(cost_per_ton = round(cost_per_ton,2)) %>%
#   select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`) %>%
#   filter(!(grepl('No 24x7 Purchase',TechSensitivity)),
#          !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))
# ggplot() +
#   geom_point(data = filter(ci_lsecost_emission_temp,
#                            abs(`Shortfall price`) >0), 
#              aes(x = emission_permwh,
#                  y = value_permwh, 
#                  color = Scenario)) +
#   geom_point(data = base, 
#              aes(x = emission_permwh,
#                  y = value_permwh, 
#                  color = TechSensitivity)) +
#   scale_color_brewer(palette = 'Set1')+
#   xlab('Emission Rate (ton/MWh)') +
#   ylab('24x7 C&I Cost ($/MWh)') +
#   theme_bw() +
#   coord_cartesian(xlim = emission_limit,ylim = c(0,100)) +
#   scale_x_continuous(breaks = emission_break)+
#   scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10))+
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_Cost_emission_Trade-off_5p.png'),
#          width = 7,
#          height = 7)
# ggplot() +
#   geom_col(data = filter(cost_per_ton,
#                          abs(`Shortfall price`) >0), 
#            aes(x = round(`Target`,2),
#                y = cost_per_ton, 
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   geom_hline(data = filter(cost_per_ton,
#                            grepl('Annual 100%', TechSensitivity)), 
#              aes(yintercept = cost_per_ton, 
#                  color = TechSensitivity)) +
#   geom_hline(yintercept = 0, color = 'gray30')+
#   scale_color_brewer(palette = 'Set2')+
#   scale_fill_brewer(palette = 'Set1') +
#   xlab('Targeted CFE') +
#   ylab('Average Emission Abatement Cost ($/ton)') +
#   theme_bw() +
#   coord_cartesian(xlim = c(0.8,1),ylim = cost_per_ton_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = cost_per_ton_break) +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_abatementcost_5p.png'),
#          width = 7,
#          height = 7)
# 
# 
# 
# ci_lsecost_emission_temp = filter(ci_lsecost_emission, 
#                                   !grepl('CES',Scenario), 
#                                   grepl('^25%',Scenario))
# base = filter(reference,
#               !grepl('CES',Scenario), 
#               grepl('^25%',Scenario)) %>%
#   mutate(value_permwh = ci_lsecost/`Participated Load`,
#          emission_permwh = ci_emission_system/`Participated Load`) %>%
#   select(TechSensitivity, value_permwh, emission_permwh)
# cost_per_ton =  ci_lsecost_emission_temp %>%
#   ungroup() %>%
#   mutate(cost_per_ton = (value_permwh - base$value_permwh[1])/(base$emission_permwh[1]-emission_permwh)) %>%
#   mutate(cost_per_ton = round(cost_per_ton,2)) %>%
#   select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`) %>%
#   filter(!(grepl('No 24x7 Purchase',TechSensitivity)),
#          !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))
# ggplot() +
#   geom_point(data = filter(ci_lsecost_emission_temp,
#                            abs(`Shortfall price`) >0), 
#              aes(x = emission_permwh,
#                  y = value_permwh, 
#                  color = Scenario)) +
#   geom_point(data = base, 
#              aes(x = emission_permwh,
#                  y = value_permwh, 
#                  color = TechSensitivity)) +
#   scale_color_brewer(palette = 'Set1')+
#   xlab('Emission Rate (ton/MWh)') +
#   ylab('24x7 C&I Cost ($/MWh)') +
#   theme_bw() +
#   coord_cartesian(xlim = emission_limit,ylim = c(0,100)) +
#   scale_x_continuous(breaks = emission_break)+
#   scale_y_continuous(breaks = seq(from = 0, to = 110, by = 10))+
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=2), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_Cost_emission_Trade-off_25p.png'),
#          width = 7,
#          height = 7)
# ggplot() +
#   geom_col(data = filter(cost_per_ton,
#                          abs(`Shortfall price`) >0), 
#            aes(x = round(`Target`,2),
#                y = cost_per_ton, 
#                fill = Scenario),
#            position = 'dodge',
#            color = 'black',
#            size = 0.3,
#            width = 0.01) +
#   geom_hline(data = filter(cost_per_ton,
#                            grepl('Annual 100%', TechSensitivity)), 
#              aes(yintercept = cost_per_ton, 
#                  color = TechSensitivity)) +
#   geom_hline(yintercept = 0, color = 'gray30')+
#   scale_color_brewer(palette = 'Set2')+
#   scale_fill_brewer(palette = 'Set1') +
#   xlab('Targeted CFE') +
#   ylab('Average Emission Abatement Cost ($/ton)') +
#   theme_bw() +
#   coord_cartesian(xlim = c(0.8,1),ylim = cost_per_ton_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = cost_per_ton_break) +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CI_abatementcost_25p.png'),
#          width = 7,
#          height = 7)
