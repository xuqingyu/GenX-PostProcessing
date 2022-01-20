# CFE Score ----
cfe_score <- read_csv(paste0(temp_RunFdr,'/CompiledResults/CFE_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  filter(abs(`Shortfall price`)>1) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))
plot_cfe_score = cfe_score %>%
  filter(grepl("^10%",Scenario)) %>%
  select(Scenario, TechSensitivity, 
         `Pre-Grid CFE Score`,`Post-Grid CFE Score Local`,
         `Post-Grid CFE Score Local_n_Import`, Target) %>%
  pivot_longer(!c(Scenario, TechSensitivity, `Pre-Grid CFE Score`,Target))
ggplot(data = plot_cfe_score) +
  geom_point(aes(x = Target, y = value, color = name)) +
  facet_wrap(Scenario~.,ncol = 3)+
  theme_bw() +
  scale_color_brewer(palette = 'Set1') +
  theme(legend.position = 'bottom') +
  ylab('Post Grid CFE Score') +
  coord_cartesian(xlim = c(0.8,1), ylim = c(0.8,1)) + 
  geom_abline(intercept = 0, slope = 1, color = 'grey30',linetype = 2) +
  ggtitle(paste0('CFE Score of ', subreg)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE Score 10p.png'),
         width = 11,
         height = 8)

plot_cfe_score = cfe_score %>%
  filter(grepl("^5%",Scenario)) %>%
  select(Scenario, TechSensitivity, 
         `Pre-Grid CFE Score`,`Post-Grid CFE Score Local`,
         `Post-Grid CFE Score Local_n_Import`, Target) %>%
  pivot_longer(!c(Scenario, TechSensitivity, `Pre-Grid CFE Score`,Target))
ggplot(data = plot_cfe_score) +
  geom_point(aes(x = Target, y = value, color = name)) +
  facet_wrap(Scenario~.,ncol = 3)+
  theme_bw() +
  scale_color_brewer(palette = 'Set1') +
  theme(legend.position = 'bottom') +
  ylab('Post Grid CFE Score') +
  coord_cartesian(xlim = c(0.8,1), ylim = c(0.8,1)) + 
  geom_abline(intercept = 0, slope = 1, color = 'grey30',linetype = 2) +
  ggtitle(paste0('CFE Score of ', subreg)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE Score 5p.png'),
         width = 11,
         height = 5)
plot_cfe_score = cfe_score %>%
  filter(grepl("^25%",Scenario)) %>%
  select(Scenario, TechSensitivity, 
         `Pre-Grid CFE Score`,`Post-Grid CFE Score Local`,
         `Post-Grid CFE Score Local_n_Import`, Target) %>%
  pivot_longer(!c(Scenario, TechSensitivity, `Pre-Grid CFE Score`,Target))
ggplot(data = plot_cfe_score) +
  geom_point(aes(x = Target, y = value, color = name)) +
  facet_wrap(Scenario~.,ncol = 3)+
  theme_bw() +
  scale_color_brewer(palette = 'Set1') +
  theme(legend.position = 'bottom') +
  ylab('Post Grid CFE Score') +
  coord_cartesian(xlim = c(0.8,1), ylim = c(0.8,1)) + 
  geom_abline(intercept = 0, slope = 1, color = 'grey30',linetype = 2) +
  ggtitle(paste0('CFE Score of ', subreg)) +
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE Score 25p.png'),
         width = 11,
         height = 5)
