# System cost emission trade off ----
system_cost <- read_csv(paste0(temp_RunFdr, '/CompiledResults/',
                               subreg,'/System_Cost_long_',subreg,'.csv'),
                        col_types = cols()) %>%
  select(case,year,Scenario,TechSensitivity,`Cost Type`,value) %>%
  group_by(case,year,Scenario,TechSensitivity) %>%
  summarize(value = sum(value)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list), 
         Scenario = factor(Scenario, levels = y_list))

system_emission = read_csv(paste0(temp_RunFdr,'/CompiledResults/system_emissions.csv'), 
                           col_types = cols()) %>%
  select(case,year,Scenario,TechSensitivity,emission_local_n_import) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list), 
         Scenario = factor(Scenario, levels = y_list))

system_cost_emission = left_join(system_cost,system_emission) %>%
  left_join(cfe_score) %>%
  select(case, year, Scenario, TechSensitivity, value, emission_local_n_import,`Post-Grid CFE Score Local_n_Import`,`Target`,`Shortfall price`)


No24rows = which(system_cost_emission$TechSensitivity == 'No 24x7 Purchase')
system_cost_emission$Target[No24rows] <- system_cost_emission$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(system_cost_emission$TechSensitivity == 'Annual 100%')
system_cost_emission$Target[Annualrows] <- system_cost_emission$`Post-Grid CFE Score Local_n_Import`[Annualrows]
system_cost_emission <- system_cost_emission %>%
  arrange(Target)


if (Studyregion == 'WECC') {
  emission_limit = c(50,30)
  emission_limit_ces = c(0,20)
  emission_break = seq(from = 50, to = 30, by = -2)
  cost_limit = c(7.5, 9)
  cost_limit_ces = c(9, 9.75)
  cost_break = seq(0,10,by = 0.1)
  cost_break_ces = seq(0,10,by = .1)
  cost_per_ton_limit = c(0,150)
  cost_per_ton_break = seq(0,160, 20)
  cost_per_ton_limit_ces = c(-100,350)
  cost_per_ton_break_ces = seq(-100,350, 50)
} else {
  emission_limit = c(230,165)
  emission_limit_ces = c(15,80)
  emission_break = seq(from = 300, to = 0, by = -10)
  cost_limit = c(33.5,42)
  cost_limit_ces = c(38,41)
  cost_break = seq(30,42,by = 2)
  cost_break_ces = seq(30,42,by = .4)
  cost_per_ton_limit = c(0,200)
  cost_per_ton_break = seq(0,200, 40)
  cost_per_ton_limit_ces = c(-200,250)
  cost_per_ton_break_ces = seq(-200,350, 50)
}


system_cost_emission_temp = filter(system_cost_emission, 
                                   !grepl('CES|Hi.|Ex.|45Q',Scenario),
                                   grepl('^10%',Scenario))
system_cost_emission_temp_bm = filter(reference, 
                                     !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                     grepl('^10%',Scenario)) %>%
  select(system_emission, systemcost,Scenario,TechSensitivity)

base = filter(reference,
              !grepl('CES|Hi.|Ex.|45Q',Scenario),
              grepl('^10%',Scenario),
              TechSensitivity == 'No 24x7 Purchase') %>%
  rename(system_cost_base = systemcost, system_emission_base = system_emission) %>%
  select(Scenario,  system_cost_base, system_emission_base)
cost_per_ton =  system_cost_emission_temp %>%
  ungroup() %>%
  left_join(base)%>%
  mutate(cost_per_ton = (value - system_cost_base)/(system_emission_base-emission_local_n_import)) %>%
  mutate(cost_per_ton = round(cost_per_ton,2)) %>%
  select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`)%>%
  filter(!(grepl('No 24x7 Purchase',TechSensitivity))) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))

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
  coord_cartesian( ylim = cost_per_ton_limit) +
  scale_y_continuous(breaks = cost_per_ton_break) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme(legend.position = 'none') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_abatementcost_10p_new.png'),
         width = 6,
         height = 6)


ggplot() +
  geom_point(data = filter(system_cost_emission_temp,
                           abs(`Shortfall price`)>0), 
             aes(x = as.numeric(emission_local_n_import)/1e6,
                 y = value/1e9, color = Scenario)) +
  geom_point(data = system_cost_emission_temp_bm, 
             aes(x = as.numeric(system_emission)/1e6,
                 y = as.numeric(systemcost)/1e9, color = TechSensitivity)) +
  scale_color_brewer(palette = 'Set1')+
  xlab('Emission (Mtons)') +
  ylab('System Cost (B$)') +
  theme_bw() +
  coord_cartesian(xlim = emission_limit,ylim = cost_limit) +
  scale_x_reverse(breaks = emission_break)+
  scale_y_continuous(breaks = cost_break)+
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=2)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Cost_emission_Trade-off_10p.png'),
         width = 6,
         height = 6,
         device = 'png')

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
  coord_cartesian(xlim = c(0.8,1), ylim = cost_per_ton_limit) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = cost_per_ton_break) +
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_abatementcost_10p.png'),
         width = 6,
         height = 6)


system_cost_emission_temp = filter(system_cost_emission, 
                                   !grepl('CES|Hi.|Ex.|45Q',Scenario),
                                   grepl('^5%',Scenario))
system_cost_emission_temp_bm = filter(reference, 
                                      !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                      grepl('^5%',Scenario)) %>%
  select(system_emission, systemcost,Scenario,TechSensitivity)

base = filter(reference,
              !grepl('CES|Hi.|Ex.|45Q',Scenario),
              grepl('^5%',Scenario),
              TechSensitivity == 'No 24x7 Purchase') %>%
  rename(system_cost_base = systemcost, system_emission_base = system_emission) %>%
  select(Scenario,  system_cost_base, system_emission_base)
cost_per_ton =  system_cost_emission_temp %>%
  ungroup() %>%
  left_join(base)%>%
  mutate(cost_per_ton = (value - system_cost_base)/(system_emission_base-emission_local_n_import)) %>%
  mutate(cost_per_ton = round(cost_per_ton,2)) %>%
  select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`)%>%
  filter(!(grepl('No 24x7 Purchase',TechSensitivity))) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))

ggplot() +
  geom_point(data = filter(system_cost_emission_temp,
                           abs(`Shortfall price`)>0), 
             aes(x = as.numeric(emission_local_n_import)/1e6,
                 y = value/1e9, color = Scenario)) +
  geom_point(data = system_cost_emission_temp_bm, 
             aes(x = as.numeric(system_emission)/1e6,
                 y = as.numeric(systemcost)/1e9, color = TechSensitivity)) +
  scale_color_brewer(palette = 'Set1')+
  xlab('Emission (Mtons)') +
  ylab('System Cost (B$)') +
  theme_bw() +
  coord_cartesian(xlim = emission_limit,ylim = cost_limit) +
  scale_x_reverse(breaks = emission_break)+
  scale_y_continuous(breaks = cost_break)+
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=2)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Cost_emission_Trade-off_5p.png'),
         width = 6,
         height = 6,
         device = 'png')

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
  coord_cartesian(xlim = c(0.8,1), ylim = cost_per_ton_limit) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = cost_per_ton_break) +
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_abatementcost_5p.png'),
         width = 6,
         height = 6)

system_cost_emission_temp = filter(system_cost_emission, 
                                   !grepl('CES|Hi.|Ex.|45Q',Scenario),
                                   grepl('^25%',Scenario))
system_cost_emission_temp_bm = filter(reference, 
                                      !grepl('CES|Hi.|Ex.|45Q',Scenario), 
                                      grepl('^25%',Scenario)) %>%
  select(system_emission, systemcost,Scenario,TechSensitivity)

base = filter(reference,
              !grepl('CES|Hi.|Ex.|45Q',Scenario),
              grepl('^25%',Scenario),
              TechSensitivity == 'No 24x7 Purchase') %>%
  rename(system_cost_base = systemcost, system_emission_base = system_emission) %>%
  select(Scenario,  system_cost_base, system_emission_base)
cost_per_ton =  system_cost_emission_temp %>%
  ungroup() %>%
  left_join(base)%>%
  mutate(cost_per_ton = (value - system_cost_base)/(system_emission_base-emission_local_n_import)) %>%
  mutate(cost_per_ton = round(cost_per_ton,2)) %>%
  select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`)%>%
  filter(!(grepl('No 24x7 Purchase',TechSensitivity))) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))

ggplot() +
  geom_point(data = filter(system_cost_emission_temp,
                           abs(`Shortfall price`)>0), 
             aes(x = as.numeric(emission_local_n_import)/1e6,
                 y = value/1e9, color = Scenario)) +
  geom_point(data = system_cost_emission_temp_bm, 
             aes(x = as.numeric(system_emission)/1e6,
                 y = as.numeric(systemcost)/1e9, color = TechSensitivity)) +
  scale_color_brewer(palette = 'Set1')+
  xlab('Emission (Mtons)') +
  ylab('System Cost (B$)') +
  theme_bw() +
  coord_cartesian(xlim = emission_limit,ylim = cost_limit) +
  scale_x_reverse(breaks = emission_break)+
  scale_y_continuous(breaks = cost_break)+
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=2)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Cost_emission_Trade-off_25p.png'),
         width = 6,
         height = 6,
         device = 'png')

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
  coord_cartesian(xlim = c(0.8,1), ylim = cost_per_ton_limit) +
  scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
  scale_y_continuous(breaks = cost_per_ton_break) +
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_abatementcost_25p.png'),
         width = 6,
         height = 6)


# system_cost_emission_temp = filter(system_cost_emission, 
#                                    grepl('CES',Scenario), 
#                                    grepl('^10%',Scenario))
# base = filter(reference,
#               grepl('CES',Scenario), 
#               grepl('^10%',Scenario)) %>%
#   select(TechSensitivity, systemcost, system_emission)
# cost_per_ton =  system_cost_emission_temp %>%
#   ungroup() %>%
#   mutate(cost_per_ton = (value - base$systemcost[1])/(base$system_emission[1]-emission_local_n_import)) %>%
#   mutate(cost_per_ton = round(cost_per_ton,2)) %>%
#   select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`)%>%
#   filter(!(grepl('No 24x7 Purchase',TechSensitivity)),
#          !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))
# 
# ggplot() +
#   geom_point(data = filter(system_cost_emission_temp,
#                            abs(`Shortfall price`)>0), 
#              aes(x = as.numeric(emission_local_n_import)/1e6,
#                  y = value/1e9, color = Scenario)) +
#   geom_point(data = base, 
#              aes(x = as.numeric(system_emission)/1e6,
#                  y = systemcost/1e9, color = TechSensitivity)) +
#   scale_color_brewer(palette = 'Set1')+
#   xlab('Emission (Mtons)') +
#   ylab('System Cost (B$)') +
#   theme_bw() +
#   coord_cartesian(xlim = emission_limit_ces,ylim = cost_limit_ces) +
#   scale_x_continuous(breaks = emission_break)+
#   scale_y_continuous(breaks = cost_break_ces)+
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=2)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Cost_emission_Trade-off_10p_ces.png'),
#          width = 7,
#          height = 7,
#          device = 'png')
# 
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
#   coord_cartesian(xlim = c(0.8,1), ylim = cost_per_ton_limit_ces) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = cost_per_ton_break_ces) +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_abatementcost_10p_ces.png'),
#          width = 7,
#          height = 7)
# 
# system_cost_emission_temp = filter(system_cost_emission, 
#                                    !grepl('CES',Scenario), 
#                                    grepl('^5%',Scenario))
# base = filter(reference,
#               !grepl('CES',Scenario), 
#               grepl('^5%',Scenario)) %>%
#   select(TechSensitivity, systemcost, system_emission)
# cost_per_ton =  system_cost_emission_temp %>%
#   ungroup() %>%
#   mutate(cost_per_ton = (value - base$systemcost[1])/(base$system_emission[1]-emission_local_n_import)) %>%
#   mutate(cost_per_ton = round(cost_per_ton,2)) %>%
#   select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`)%>%
#   filter(!(grepl('No 24x7 Purchase',TechSensitivity)),
#          !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))
# 
# ggplot() +
#   geom_point(data = filter(system_cost_emission_temp,
#                            abs(`Shortfall price`)>0), 
#              aes(x = as.numeric(emission_local_n_import)/1e6,
#                  y = value/1e9, color = Scenario)) +
#   geom_point(data = base, 
#              aes(x = as.numeric(system_emission)/1e6,
#                  y = systemcost/1e9, color = TechSensitivity)) +
#   scale_color_brewer(palette = 'Set1')+
#   xlab('Emission (Mtons)') +
#   ylab('System Cost (B$)') +
#   theme_bw() +
#   coord_cartesian(xlim = emission_limit,ylim = cost_limit) +
#   scale_x_continuous(breaks = emission_break)+
#   scale_y_continuous(breaks = cost_break)+
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=2)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Cost_emission_Trade-off_5p.png'),
#          width = 7,
#          height = 7,
#          device = 'png')
# 
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
#   coord_cartesian(xlim = c(0.8,1), ylim = cost_per_ton_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = cost_per_ton_break) +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_abatementcost_5p.png'),
#          width = 7,
#          height = 7)
# 
# system_cost_emission_temp = filter(system_cost_emission, 
#                                    !grepl('CES',Scenario), 
#                                    grepl('^25%',Scenario))
# base = filter(reference,
#               !grepl('CES',Scenario), 
#               grepl('^25%',Scenario)) %>%
#   select(TechSensitivity, systemcost, system_emission)
# cost_per_ton =  system_cost_emission_temp %>%
#   ungroup() %>%
#   mutate(cost_per_ton = (value - base$systemcost[1])/(base$system_emission[1]-emission_local_n_import)) %>%
#   mutate(cost_per_ton = round(cost_per_ton,2)) %>%
#   select(Scenario, TechSensitivity, cost_per_ton,Target,`Shortfall price`)%>%
#   filter(!(grepl('No 24x7 Purchase',TechSensitivity)),
#          !(grepl('Annual 100%',TechSensitivity) & grepl('Adv. Tech.',Scenario)))
# 
# ggplot() +
#   geom_point(data = filter(system_cost_emission_temp,
#                            abs(`Shortfall price`)>0), 
#              aes(x = as.numeric(emission_local_n_import)/1e6,
#                  y = value/1e9, color = Scenario)) +
#   geom_point(data = base, 
#              aes(x = as.numeric(system_emission)/1e6,
#                  y = systemcost/1e9, color = TechSensitivity)) +
#   scale_color_brewer(palette = 'Set1')+
#   xlab('Emission (Mtons)') +
#   ylab('System Cost (B$)') +
#   theme_bw() +
#   coord_cartesian(xlim = emission_limit,ylim = cost_limit) +
#   scale_x_continuous(breaks = emission_break)+
#   scale_y_continuous(breaks = cost_break)+
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=2)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_Cost_emission_Trade-off_25p.png'),
#          width = 7,
#          height = 7,
#          device = 'png')
# 
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
#   coord_cartesian(xlim = c(0.8,1), ylim = cost_per_ton_limit) +
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = cost_per_ton_break) +
#   theme(legend.position = 'bottom') + 
#   guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1)) +
#   ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/System_abatementcost_25p.png'),
#          width = 7,
#          height = 7)
