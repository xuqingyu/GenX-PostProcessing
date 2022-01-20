# Time series ----
cfe_ts_allcase <- read_csv(paste0(temp_RunFdr,"/CompiledResults/CFE_timeseries.csv"))

cfe_modified_load <- cfe_ts_allcase %>%
  filter(name %in% c('Load','Storage Discharge','Storage Charge','DR Load Increase','DR Load Decrease')) %>%
  pivot_wider(names_from = 'name',values_from = 'value') %>%
  mutate(`Modified Load` = Load + `Storage Charge` + `DR Load Increase` - `Storage Discharge` - `DR Load Decrease`) %>%
  select(case, year, `policy`, Time_index, Scenario, TechSensitivity, `Modified Load`, Load) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list))
cfe_generation <- cfe_ts_allcase %>%
  filter((name %in% fuel_list)) %>%
  select(case, year, `policy`, Time_index, Scenario, TechSensitivity, name, value)%>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list))

start_period = 2
starting_time_index = 168*(start_period-1)+1
end_time_index = starting_time_index+167
temp_cfe_generation <- filter(cfe_generation,
                              grepl('^10%',Scenario),
                              !grepl('CES|Hi.|Ex.|45Q',Scenario),
                              Time_index %in% c(starting_time_index:end_time_index))
temp_cfe_modified_load <- filter(cfe_modified_load,
                                 grepl('^10%',Scenario), 
                                 !grepl('CES|Hi.|Ex.|45Q',Scenario),
                                 Time_index %in% c(starting_time_index:end_time_index)) %>%
  pivot_longer(c(`Modified Load`, Load),names_to = 'Load Type', values_to = 'Load')

if (Studyregion == 'WECC') {
  ts_limit_10p = c(0,6)
  ts_break_10p = seq(from = 0, to = 6, by = 1)
} else {
  ts_limit_10p = c(0,24)
  ts_break_10p = seq(from = 0, to = 80, by = 4)
}

ggplot() +
  geom_area(data = filter(temp_cfe_generation,
                          grepl('Hourly 98%',TechSensitivity)), 
            aes(x = Time_index, 
                y = value/1e3, 
                fill = factor(name, levels = capacity_resource_levels))) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  geom_line(data = filter(temp_cfe_modified_load,
                          grepl('Hourly 98%', TechSensitivity)), 
            aes(x = Time_index,
                y = Load/1e3, linetype = `Load Type`)) +
  # scale_color_brewer(palette = 'Set1')+
  ylab('Generation/Load (GW)') +
  xlab('Time Index')+
  facet_wrap(.~Scenario, ncol = 1) +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = ts_limit_10p)+
  scale_y_continuous(breaks = ts_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  # ggtitle(paste0('24x7 CFE hourly matching pattern of example week \n',subreg))+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_MatchingPattern_98.png'),
         width = 4,
         height = 9)
ggplot() +
  geom_area(data = filter(temp_cfe_generation,
                          grepl('Hourly 100%',TechSensitivity)), 
            aes(x = Time_index, 
                y = value/1e3, 
                fill = factor(name, levels = capacity_resource_levels))) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  geom_line(data = filter(temp_cfe_modified_load,
                          grepl('Hourly 100%', TechSensitivity)), 
            aes(x = Time_index,
                y = Load/1e3, linetype = `Load Type`)) +
  # scale_color_brewer(palette = 'Set1')+
  ylab('Generation/Load (GW)') +
  xlab('Time Index')+
  facet_wrap(.~Scenario, ncol = 1) +
  geom_hline(yintercept = 0, color = 'grey30')+
  coord_cartesian(ylim = ts_limit_10p)+
  scale_y_continuous(breaks = ts_break_10p)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  # ggtitle(paste0('24x7 CFE hourly matching pattern of example week \n',subreg))+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_MatchingPattern_100.png'),
         width = 4,
         height = 9)
