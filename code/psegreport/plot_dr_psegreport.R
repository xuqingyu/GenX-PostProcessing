
k=2
load_timeseries_fn <- paste0(RunFdr,'/CompiledResults/',Subregions[k],'/Load/Load_fullyear_timeseries_',Subregions[k],'.csv')
load_timeseries <- read_csv(load_timeseries_fn, col_types = cols())
temp <- load_timeseries %>%
  filter(case == 'statedpolicy_mid', year == 2050)

start_period = 1
starting_time_index = 168*(start_period-1)+1
end_time_index = starting_time_index+167


temp_plot <- temp %>% filter(HourID %in% c(starting_time_index:end_time_index))
temp_plot_total <- temp_plot %>%
  group_by(HourID) %>%
  summarize(MW = sum(MW)) %>%
  mutate(Load_Type = 'Load after Demand Response')
ggplot() +
  geom_area(data=temp_plot,aes(x=HourID, y=MW/1e3, fill=Load_Type))+
  geom_line(data=temp_plot_total,aes(x=HourID, y=MW/1e3, linetype = Load_Type),color="black") +
  scale_fill_brewer(palette = 'Set1')+
  theme_bw() +
  ylab('Load (GW)') +
  geom_hline(yintercept = 0, color = 'gray30') +
  theme(legend.position = 'bottom') +
  ggtitle(paste0('Load of ',Subregions[k], ' of the #', start_period, ' week of the Year 2050')) +
  ggsave(paste0(RunFdr,'/CompiledResults/',Subregions[k],'/Graphics/example_dr.eps'),
         width = 10,
         height = 5,
         device = 'eps')
  


