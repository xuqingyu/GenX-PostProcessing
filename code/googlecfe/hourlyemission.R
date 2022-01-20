
# hourly emission ----
hourly_emission <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_emissionrate_timeseries_full.csv'))
plot_hourly_emission <- hourly_emission %>%
  filter(Scenario == "10% CI Part., Curt. Tech., 80% CES",
         TechSensitivity == 'Hourly 100%') %>%
  mutate(Diff = `Local Emission Rate` - `Local and Import Emission Rate`)%>%
  pivot_longer(!c(HourID, case, year, Scenario, TechSensitivity)) %>%
  filter(name %in% c('Local Emission Rate','Local and Import Emission Rate','Diff')) %>%
  mutate(name = factor(name, levels = c('Local Emission Rate','Local and Import Emission Rate','Diff')))
ggplot(data = plot_hourly_emission) +
  geom_line(aes(x = HourID, y = value)) +
  facet_grid(name~.) +
  theme_bw() +
  geom_hline(yintercept = 0, color = 'grey30') +
  ggtitle(paste0('Hourly emissions of 10% CI Participation, Current Technology, 100 hourly matching', subreg))+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/Hourly_emission.png'),
         width = 16,
         height = 7)
