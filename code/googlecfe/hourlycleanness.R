# hourly cleanness ----
hourly_cleanness <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_cleanness_timeseries_full.csv'))
plot_hourly_cleanness <- hourly_cleanness %>%
  filter(Scenario == "10% CI Part., Curt. Tech., 80% CES",
         TechSensitivity == 'Hourly 90%') %>%
  mutate(Diff = `Rest of Local Cleanness` - `Rest of Local and Import Cleanness`)%>%
  pivot_longer(!c(HourID, case, year, Scenario, TechSensitivity)) %>%
  filter(name %in% c('Rest of Local and Import Cleanness','Rest of Local Cleanness','Diff')) %>%
  mutate(name = factor(name, levels = c('Rest of Local Cleanness','Rest of Local and Import Cleanness','Diff')))
ggplot(data = plot_hourly_cleanness) +
  geom_line(aes(x = HourID, y = value)) +
  facet_grid(name~.) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'grey30') +
  ggtitle(paste0('Hourly cleanness of 10% CI Participation, Current Technology, 80% Federal CES, 90% hourly matching of ', subreg))+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Hourly_cleanness.png'),
         width = 16,
         height = 7)
