# C&I emissions ----
cfe_score <- read_csv(paste0(RunFdr,'/CompiledResults/tfs_cfe_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))
cfe_emission_table_withname = read_csv(paste0(RunFdr,'/CompiledResults/tfs_part_emissions.csv'), 
                                       col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list), 
         Scenario = factor(Scenario, levels = y_list)) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity','Policy'))%>%
  mutate(emission_permwh = emission_measure/(`Load` + `Storage loss`))


if (Studyregion == 'WECC') {
  emission_limit = c(0,0.2)
  emission_break = seq(0,0.2, 0.02)
  am_position = 0.13
  nocip_position = 0.165
  nocip_cfe = "70%"
} else {
  emission_limit = c(0,0.4)
  emission_break = seq(0,0.4, 0.04)
  am_position = 0.20
  nocip_position = 0.38
  nocip_cfe = "40%"
}

No24rows = which(cfe_emission_table_withname$TechSensitivity == 'No 24x7 Purchase')
cfe_emission_table_withname$Target[No24rows] <- cfe_emission_table_withname$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_emission_table_withname$TechSensitivity == 'Annual 100%')
cfe_emission_table_withname$Target[Annualrows] <- cfe_emission_table_withname$`Post-Grid CFE Score Local_n_Import`[Annualrows]
#troubling rows
if (Studyregion == 'PJM') {
  troublerows = which(cfe_emission_table_withname$TechSensitivity == 'Annual 100%' & 
                        cfe_emission_table_withname$Scenario == '10% CI Part., Adv. Tech. no Comb.')
  cfe_emission_table_withname$Target[troublerows] <- 0.74
}
cfe_emission_table_withname <- cfe_emission_table_withname %>%
  arrange(Target)

ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^10%',Scenario),
                         !grepl('CES', Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                         TechSensitivity != 'No 24x7 Purchase',
                         (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = emission_measure/as.numeric(`Participated Load`),
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference,
                           grepl('^10%',Scenario),
                           !grepl('CES', Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = ci_emission_system/as.numeric(`Participated Load`),
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3) +
  annotate("text", x = 1, y = am_position, label = "100% \nAnnual \nMatching",size = 3) +
  annotate("text", x = 5, y = nocip_position, label = paste0("Reference Case, CFE Score = ",nocip_cfe),size = 3) +
  geom_hline(yintercept = 0, color = 'black') +
  coord_cartesian(ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break) +
  labs(x = "CFE Score", y = "Emission Rate (ton/MWh)")+
  theme_bw() + 
  theme(legend.position = c(0.7,0.6),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_10p_new.png'),
         width = 6,
         height = 6)

ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^5%',Scenario),
                         !grepl('CES', Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                         TechSensitivity != 'No 24x7 Purchase',
                         (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = emission_measure/as.numeric(`Participated Load`),
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference,
                           grepl('^5%',Scenario),
                           !grepl('CES', Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = ci_emission_system/as.numeric(`Participated Load`),
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3) +
  annotate("text", x = 1, y = am_position, label = "100% \nAnnual \nMatching",size = 3) +
  annotate("text", x = 5, y = nocip_position, label = paste0("Reference Case, CFE Score = ",nocip_cfe),size = 3) +
  geom_hline(yintercept = 0, color = 'black') +
  coord_cartesian(ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break) +
  labs(x = "CFE Score", y = "Emission Rate (ton/MWh)")+
  theme_bw() + 
  theme(legend.position = c(0.7,0.6),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_5p_new.png'),
         width = 6,
         height = 6)

ggplot()+
  geom_col(data = filter(cfe_emission_table_withname,
                         grepl('^25%',Scenario),
                         !grepl('CES', Scenario),
                         !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                         TechSensitivity != 'No 24x7 Purchase',
                         (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')),
           aes(x = as.character(formatC(round(Target,2),format = 'f',digit = 2)), 
               y = emission_measure/as.numeric(`Participated Load`),
               fill = Scenario),
           position = 'dodge',
           color = 'black',
           size = 0.3) +
  scale_fill_brewer(palette = 'Set1') +
  geom_hline(data = filter(reference,
                           grepl('^25%',Scenario),
                           !grepl('CES', Scenario),
                           !grepl('Hi. Nat. Gas P.|45Q| Ex. Limit', Scenario),
                           TechSensitivity == 'No 24x7 Purchase'),
             aes(yintercept = ci_emission_system/as.numeric(`Participated Load`),
                 color = TechSensitivity),
             show.legend = F) +
  scale_color_brewer(palette = 'Set2') +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3) +
  annotate("text", x = 1, y = am_position, label = "100% \nAnnual \nMatching",size = 3) +
  annotate("text", x = 5, y = nocip_position, label = paste0("Reference Case, CFE Score = ",nocip_cfe),size = 3) +
  geom_hline(yintercept = 0, color = 'black') +
  coord_cartesian(ylim = emission_limit) +
  scale_y_continuous(breaks = emission_break) +
  labs(x = "CFE Score", y = "Emission Rate (ton/MWh)")+
  theme_bw() + 
  theme(legend.position = c(0.7,0.6),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) + 
  guides(color=guide_legend(ncol=1), fill=guide_legend(ncol=1))+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/Emission_comparison_25p_new.png'),
         width = 6,
         height = 6)

