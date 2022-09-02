# System emissions ----
cfe_score <- read_csv(paste0(RunFdr,'/CompiledResults/tfs_cfe_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))

cfe_emission_table_withname = read_csv(paste0(RunFdr,
                                              '/CompiledResults/tfs_system_emissions.csv'), 
                                       col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list), 
         Scenario = factor(Scenario, levels = y_list)) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity'))
if (Studyregion == 'WECC') {
  emission_limit = c(30,50)
  emission_limit_ces = c(0,20)
  emission_break = seq(from = 0, to = 50, by = 2)
  am_position = 46
  nocip_position = 45
  nocip_cfe = "70%"
  legendposition = c(0.77, 0.93)
} else {
  emission_limit = c(300,320)
  emission_limit_ces = c(0,80)
  emission_break = seq(from = 0, to = 350, by = 2)
  am_position = 310
  nocip_position = 317
  nocip_cfe = "40%"
  legendposition = c(0.77, 0.93)
}

No24rows = which(cfe_emission_table_withname$TechSensitivity == 'No 24x7 Purchase')
cfe_emission_table_withname$Target[No24rows] <- cfe_emission_table_withname$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_emission_table_withname$TechSensitivity == 'Annual 100%')
cfe_emission_table_withname$Target[Annualrows] <- cfe_emission_table_withname$`Post-Grid CFE Score Local_n_Import`[Annualrows]

cfe_emission_table_withname <- cfe_emission_table_withname %>%
  arrange(Target)

if (Studyregion == 'PJM') {
  troublerows = which(cfe_emission_table_withname$TechSensitivity == 'Annual 100%' & 
                        cfe_emission_table_withname$Scenario == '10% CI Part., Adv. Tech. no Comb.')
  cfe_emission_table_withname$Target[troublerows] <- 0.74
}






ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^10%',Scenario),
                         !grepl('CES|Hi.|45Q|Ex.', Scenario),
                         TechSensitivity != 'No 24x7 Purchase',
                         (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = (as.numeric(`emission_local_n_import`))/1e6,
               fill = Scenario),
           color = 'black',
           size = 0.3,
           position = 'dodge')  +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  geom_hline(data = filter(reference,
                           grepl('^10%',Scenario),
                           grepl('Curt.',Scenario),
                           !grepl('CES|Hi.|45Q|Ex.', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = (as.numeric(`system_emission`))/1e6,
                 color = TechSensitivity),
             show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  annotate("text", x = 1, y = am_position, label = "100% \nAnnual \nMatching",size = 3) +
  annotate("text", x = 4, y = nocip_position, label = paste0("Reference Case, CFE Score = ",nocip_cfe),size = 3) +
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian( ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission (Mtons)') +
  xlab('CFE Score') +
  theme(legend.position = c(0.77,0.90),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) + 
  guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1)) +
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/',subreg,'_System_Emission_comparison_10p_new.png'),
         width = 6,
         height = 6)
ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^25%',Scenario),
                         !grepl('CES|Hi.|45Q|Ex.', Scenario),
                         TechSensitivity != 'No 24x7 Purchase',
                         (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = (as.numeric(`emission_local_n_import`))/1e6,
               fill = Scenario),
           color = 'black',
           size = 0.3,
           position = 'dodge')  +
  scale_fill_brewer(palette = 'Set1') +
  theme_bw() +
  geom_hline(data = filter(reference,
                           grepl('^25%',Scenario),
                           grepl('Curt.',Scenario),
                           !grepl('CES|Hi.|45Q|Ex.', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = (as.numeric(`system_emission`))/1e6,
                 color = TechSensitivity),
             show.legend = F) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  annotate("text", x = 1, y = am_position, label = "100% \nAnnual \nMatching",size = 3) +
  annotate("text", x = 4, y = nocip_position, label = paste0("Reference Case, CFE Score = ",nocip_cfe),size = 3) +
  scale_color_brewer(palette = 'Set2') +
  coord_cartesian( ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break)+
  ylab('Emission (Mtons)') +
  xlab('CFE Score') +
  theme(legend.position = c(0.77,0.90),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) + 
  guides(fill=guide_legend(ncol=1),color=guide_legend(ncol=1)) +
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/',subreg,'_System_Emission_comparison_25p_new.png'),
         width = 6,
         height = 6)



