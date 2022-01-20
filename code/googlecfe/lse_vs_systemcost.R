cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))

ci_cost <- read_csv(paste0(temp_RunFdr,'/CompiledResults/ci_lse_cost.csv')) %>%
  group_by(case,year,Scenario, TechSensitivity) %>%
  summarise(value = sum(value)) %>%
  rename(ci_lsecost = value) %>%
  select(case,year,Scenario, TechSensitivity, ci_lsecost) %>%
  filter(!(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'))) 
system_cost <- read_csv(paste0(temp_RunFdr, '/CompiledResults/',
                               subreg,'/System_Cost_long_',subreg,'.csv'),
                        col_types = cols()) %>%
  group_by(case,year,Scenario, TechSensitivity) %>%
  summarise(value = sum(value)) %>%
  rename(systemcost = value) %>%
  select(case,year,Scenario, TechSensitivity, systemcost) %>%
  filter(!(TechSensitivity %in% c('No 24x7 Purchase', 'Annual 100%'))) 
baseline = reference %>%
  filter(TechSensitivity == 'No 24x7 Purchase') %>%
  rename(base_ci_lsecost = ci_lsecost,
         base_systemcost = systemcost) %>%
  select(Scenario, base_ci_lsecost,base_systemcost)

cost_compare <- ci_cost %>%
  left_join(system_cost) %>%
  left_join(cfe_score) %>%
  left_join(baseline) %>%
  mutate(ci_lsecost = ci_lsecost - base_ci_lsecost,
         systemcost = systemcost - base_systemcost) %>%
  mutate(rest_system_increase = systemcost - ci_lsecost) %>%
  select(case,year, Scenario, TechSensitivity,ci_lsecost,systemcost,rest_system_increase, Target, `Shortfall price`) %>%
  pivot_longer(cols = c(ci_lsecost, systemcost,rest_system_increase)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list))

ggplot() +
  geom_col(data = filter(cost_compare,
                         abs(`Shortfall price`) >0,
                         name == 'rest_system_increase',
                         !grepl('45Q|Hi.|Ex.|CES', Scenario)),
           aes(x = round(`Target`,2),
               y = value/1e6, 
               fill = name),
           position = 'dodge',
           color = 'black',
           size = 0.3,
           width = 0.01) +
  facet_wrap(.~Scenario) +
  ylab('Cost to Rest of California (M$/yr)') +
  xlab('Target CFE') +
  theme_bw() +  
  theme(legend.position = 'None')+
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(yintercept = 0, color = 'gray30') + 
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CosttoRestofsystem.png'),
         width = 7,
         height = 7)
