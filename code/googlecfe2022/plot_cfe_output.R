# Generation Plot ----
cfe_score <- read_csv(paste0(RunFdr,'/CompiledResults/tfs_cfe_table.csv'),
                      col_types = cols()) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list),
         Scenario = factor(Scenario, levels = y_list)) %>%
  mutate(`Shortfall price` = round(`Shortfall price`,3),
         `Excess price` = round(`Excess price`,3)) %>%
  mutate(`Target` = 1- `Shortfall`/(`Load` + `Storage loss`))
cfe_gen_allcase <- read_csv(paste0(RunFdr,"/CompiledResults/",subreg,"/Generation/tfs_gen_output_",subreg,".csv"),
                            col_types = cols()) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity','Policy')) %>%
  mutate(`Post-Grid CFE Score Local_n_Import` = round(as.numeric(`Post-Grid CFE Score Local_n_Import`),3)) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  na.omit()

No24rows = which(cfe_gen_allcase$TechSensitivity == 'No 24x7 Purchase')
cfe_gen_allcase$Target[No24rows] <- cfe_gen_allcase$`Post-Grid CFE Score Local_n_Import`[No24rows]
Annualrows = which(cfe_gen_allcase$TechSensitivity == 'Annual 100%')
cfe_gen_allcase$Target[Annualrows] <- cfe_gen_allcase$`Post-Grid CFE Score Local_n_Import`[Annualrows]
if (Studyregion == 'PJM') {
  troublerows = which(cfe_gen_allcase$TechSensitivity == 'Annual 100%' & 
                        cfe_gen_allcase$Scenario == '10% CI Part., Adv. Tech. no Comb.')
  cfe_gen_allcase$Target[troublerows] <- 0.74
}
cfe_gen_allcase <- cfe_gen_allcase %>%
  arrange(Target)


p_width_all = 12
p_height_all = 6

p_width_ref = 4
p_height_ref = 6

if (Studyregion == 'WECC') {
  ci_load_before_time_reduction = 192.9 
  cfe_gen_allcase = cfe_gen_allcase %>%
    filter(!(Fuel == 'Adv. Nuclear'))
  output_limit_5p = c(0,15)
  output_break_5p = seq(from = 0, to = 15, by = 2)
  nudge_5p = 0.75
  output_limit_10p = c(0,30)
  output_break_10p = seq(from = 0, to = 30, by = 4)
  nudge_10p = 1.5
  nudge_10p_am = 5
  output_limit_25p = c(0,70)
  output_break_25p = seq(from = 0, to = 70, by = 10)
  nudge_25p = 4.5
} else {
  ci_load_before_time_reduction = 565.90
  output_limit_5p = c(0,40)
  output_break_5p = seq(from = 0, to = 40, by = 4)
  nudge_5p = 2
  output_limit_10p = c(0,80)
  output_break_10p = seq(from = 0, to = 80, by = 10)
  nudge_10p = 3
  nudge_10p_am = 8
  output_limit_25p = c(0,240)
  output_break_25p = seq(from = 0, to = 240, by = 50)
  nudge_25p = 12
}

cfe_gen_allcase_total = cfe_gen_allcase %>%
  group_by(Scenario, TechSensitivity, Target, `Post-Grid CFE Score Local_n_Import`, `Shortfall price`) %>%
  summarize(AnnualOutput = sum(AnnualOutput))

# Procurement as Ratio of Annual Participating Demand
ggplot(data = filter(cfe_gen_allcase_total, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario),
                     TechSensitivity != 'No 24x7 Purchase'))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = AnnualOutput/ci_load_before_time_reduction/1e6/0.1,
               fill = Scenario),
           colour="black", size= 0.1, position = 'dodge') +
  scale_fill_brewer(palette = 'Set1')+
  scale_y_continuous(limits = c(0,1.4),breaks = seq(0,1.4,0.1)) +
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  xlab('CFE Score')+
  ylab('Procurement as Ratio of Annual Participating Demand') +
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  theme(legend.position = 'none')+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_10p_new_norm.png'),
         width = 6,
         height = 6)

ggplot(data = filter(cfe_gen_allcase, 
                     grepl('^10%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario),
                     TechSensitivity != 'No 24x7 Purchase',
                     (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = AnnualOutput/1e6, 
               fill=Fuel),
           colour="black", size= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.1),
             colour="black", size= .5) +
  annotate("text", x = 7, y = ci_load_before_time_reduction*.1+nudge_10p,label = 'Annual Demand')+
  annotate("text", x = 1, y = ci_load_before_time_reduction*.1+nudge_10p_am, label = "100% \nAnnual \nMatching",size = 3) +
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  coord_cartesian(ylim = output_limit_10p)+
  scale_y_continuous(breaks = output_break_10p) +
  labs(x = 'CFE Score', y = 'Generation Output (TWh)')+
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme_bw() +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_blank()) + 
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_10p_new.png'),
         width = 6,
         height = 10)

ggplot(data = filter(cfe_gen_allcase, 
                     grepl('^10%',Scenario),
                     grepl('Hi.',Scenario),
                     TechSensitivity != 'No 24x7 Purchase',
                     (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = AnnualOutput/1e6, 
               fill=Fuel),
           colour="black", size= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.1),
             colour="black", size= .5) +
  geom_text(aes(x = 7, y = ci_load_before_time_reduction*.1+nudge_10p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  coord_cartesian(ylim = output_limit_10p)+
  scale_y_continuous(breaks = output_break_10p) +
  xlab('CFE Score')+
  ylab('Generation Output (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_10p_new_highnaturalgasprice.png'),
         width = 15,
         height = 5)
ggplot(data = filter(cfe_gen_allcase, 
                     grepl('^10%',Scenario),
                     grepl('45Q',Scenario),
                     TechSensitivity != 'No 24x7 Purchase',
                     (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = AnnualOutput/1e6, 
               fill=Fuel),
           colour="black", size= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.1),
             colour="black", size= .5) +
  geom_text(aes(x = 7, y = ci_load_before_time_reduction*.1+nudge_10p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  coord_cartesian(ylim = output_limit_10p)+
  scale_y_continuous(breaks = output_break_10p) +
  xlab('CFE Score')+
  ylab('Generation Output (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_10p_new_no45Q.png'),
         width = 15,
         height = 5)
ggplot(data = filter(cfe_gen_allcase, 
                     grepl('^10%',Scenario),
                     grepl('Ex.',Scenario),
                     TechSensitivity != 'No 24x7 Purchase',
                     (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = AnnualOutput/1e6, 
               fill=Fuel),
           colour="black", size= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.1),
             colour="black", size= .5) +
  geom_text(aes(x = 7, y = ci_load_before_time_reduction*.1+nudge_10p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  coord_cartesian(ylim = output_limit_10p)+
  scale_y_continuous(breaks = output_break_10p) +
  xlab('CFE Score')+
  ylab('Generation Output (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_10p_new_noExcessLimit.png'),
         width = 15,
         height = 5)
ggplot(data = filter(cfe_gen_allcase, 
                     grepl('^10%',Scenario),
                     grepl('CES',Scenario),
                     TechSensitivity != 'No 24x7 Purchase',
                     (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = AnnualOutput/1e6, 
               fill=Fuel),
           colour="black", size= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.1),
             colour="black", size= .5) +
  geom_text(aes(x = 7, y = ci_load_before_time_reduction*.1+nudge_10p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  coord_cartesian(ylim = output_limit_10p)+
  scale_y_continuous(breaks = output_break_10p) +
  xlab('CFE Score')+
  ylab('Generation Output (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_10p_new_with80CES.png'),
         width = 15,
         height = 5)
ggplot(data = filter(cfe_gen_allcase, 
                     grepl('^5%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario),
                     TechSensitivity != 'No 24x7 Purchase',
                     (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = AnnualOutput/1e6, 
               fill=Fuel),
           colour="black", size= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.05),
             colour="black", size= .5) +
  geom_text(aes(x = 7, y = ci_load_before_time_reduction*.05+nudge_10p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  coord_cartesian(ylim = output_limit_5p)+
  scale_y_continuous(breaks = output_break_5p) +
  xlab('CFE Score')+
  ylab('Generation Output (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_5p_new.png'),
         width = 15,
         height = 5)

ggplot(data = filter(cfe_gen_allcase, 
                     grepl('^25%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario),
                     TechSensitivity != 'No 24x7 Purchase',
                     (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
  geom_col(aes(x= as.character(formatC(round(Target,2),format = 'f',digit = 2)),
               y = AnnualOutput/1e6, 
               fill=Fuel),
           colour="black", size= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.25),
             colour="black", size= .5) +
  geom_text(aes(x = 7, y = ci_load_before_time_reduction*.25+nudge_10p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 3) +
  scale_fill_manual(name = "Resources", values = fuel_colors) + 
  coord_cartesian(ylim = output_limit_25p)+
  scale_y_continuous(breaks = output_break_25p) +
  xlab('CFE Score')+
  ylab('Generation Output (TWh)') +
  geom_hline(yintercept = 0, color = 'grey30')+
  geom_vline(xintercept = 1.5, linetype = 'dashed', size = 0.3)+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_25p_new.png'),
         width = 15,
         height = 5)

# ggplot(data = filter(cfe_gen_allcase, 
#                      grepl('^10%',Scenario),
#                      !grepl('CES',Scenario),
#                      TechSensitivity != 'No 24x7 Purchase',
#                      (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
#   geom_col(aes(x= round(`Target`,2), 
#                y = AnnualOutput/1e6, 
#                fill=Fuel),
#            colour="black", size= 0.1) +
#   geom_hline(aes(yintercept = ci_load_before_time_reduction*.1),
#              colour="black", size= .5) +
#   geom_text(aes(x = 0.85, y = ci_load_before_time_reduction*.1+nudge_10p, label = 'Annual Demand'))+
#   facet_wrap(Scenario~.,ncol = 3) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   coord_cartesian(xlim = c(0.8,1), ylim = output_limit_10p)+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = output_break_10p) +
#   xlab('Target CFE')+
#   ylab('Generation Output (TWh)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_10p_all.png'),
#          width = 12,
#          height = 9)
# ggplot(data = filter(cfe_gen_allcase, 
#                      grepl('^10%',Scenario),
#                      !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
#   geom_col(aes(x= round(`Target`,2), 
#                y = AnnualOutput/1e6, 
#                fill=Fuel),
#            colour="black", size= 0.1) +
#   geom_hline(aes(yintercept = ci_load_before_time_reduction*.1),
#              colour="black", size= .5) +
#   geom_text(aes(x = 0.85, y = ci_load_before_time_reduction*.1+nudge_10p, label = 'Annual Demand'))+
#   facet_wrap(Scenario~.,ncol = 1) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   coord_cartesian(xlim = c(0.8,1), ylim = output_limit_10p)+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = output_break_10p) +
#   xlab('Target CFE')+
#   ylab('Generation Output (TWh)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_10p.png'),
#          width = 4,
#          height = 9)

# ggplot(data = filter(cfe_gen_allcase, 
#                      grepl('^5%',Scenario),
#                      !grepl('CES|Hi.|Ex.|45Q',Scenario),
#                      TechSensitivity != 'No 24x7 Purchase',
#                      (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
#   geom_col(aes(x= round(`Target`,2), 
#                y = AnnualOutput/1e6, 
#                fill=Fuel),
#            colour="black", size= 0.1) +
#   geom_hline(aes(yintercept = ci_load_before_time_reduction*.05),
#              colour="black", size= .5) +
#   geom_text(aes(x = 0.85, y = ci_load_before_time_reduction*.05+nudge_5p, label = 'Annual Demand'))+
#   facet_wrap(Scenario~.,ncol = 1) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   coord_cartesian(xlim = c(0.8,1), ylim = output_limit_5p)+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = output_break_5p) +
#   xlab('Target CFE')+
#   ylab('Generation Output (TWh)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_5p.png'),
#          width = 4,
#          height = 9)
# ggplot(data = filter(cfe_gen_allcase, 
#                      grepl('^25%',Scenario),
#                      !grepl('CES|Hi.|Ex.|45Q',Scenario),
#                      TechSensitivity != 'No 24x7 Purchase',
#                      (abs(`Shortfall price`) > 0.1| TechSensitivity == 'Annual 100%')))+
#   geom_col(aes(x= round(`Target`,2), 
#                y = AnnualOutput/1e6, 
#                fill=Fuel),
#            colour="black", size= 0.1) +
#   geom_hline(aes(yintercept = ci_load_before_time_reduction*.25),
#              colour="black", size= .5) +
#   geom_text(aes(x = 0.85, y = ci_load_before_time_reduction*.25+nudge_25p, label = 'Annual Demand'))+
#   facet_wrap(Scenario~.,ncol = 1) +
#   scale_fill_manual(name = "Resources", values = fuel_colors) + 
#   coord_cartesian(xlim = c(0.8,1), ylim = output_limit_25p)+
#   scale_x_continuous(breaks = seq(from = 0.8, to = 1, by = 0.02)) +
#   scale_y_continuous(breaks = output_break_25p) +
#   xlab('Target CFE')+
#   ylab('Generation Output (TWh)') +
#   geom_hline(yintercept = 0, color = 'grey30')+
#   theme_bw() +
#   theme(legend.position = 'bottom')+
#   ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_25p.png'),
#          width = 4,
#          height = 9)

cfe_gen_allcase_all100 <- read_csv(paste0(RunFdr,"/CompiledResults/",subreg,"/Generation/tfs_gen_output_",subreg,".csv")) %>%
  mutate(Fuel = factor(Fuel, levels = capacity_resource_levels)) %>%
  left_join(cfe_score, by = c('case','year','Scenario','TechSensitivity')) %>%
  mutate(TechSensitivity = factor(TechSensitivity, levels = x_list)) %>%
  mutate(Scenario = factor(Scenario, levels = y_list)) %>%
  filter(TechSensitivity == 'Annual 100%')

# 
if (Studyregion == 'WECC') {
  cfe_gen_allcase_all100 = cfe_gen_allcase_all100 %>%
    filter(!(Fuel == 'Adv. Nuclear'))
}


# ggplot(data = filter(cfe_gen_allcase_all100,
#                      grepl('^10%',Scenario),
#                      !grepl('CES',Scenario),
#                      grepl('Curt. Tech.',Scenario)))+
ggplot(data = filter(cfe_gen_allcase_all100,
                     grepl('^10%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x= TechSensitivity,
               y = AnnualOutput/1e6,
               fill=Fuel),
           colour="black", size= 0.1, width= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.1),
             colour="black", size= .5) +
  geom_text(aes(x = 0.85, y = ci_load_before_time_reduction*.1+nudge_10p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('')+
  ylab('Generation Output (TWh)') +
  coord_cartesian(ylim = output_limit_10p)+
  scale_y_continuous(breaks = output_break_10p) +
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_annual100_10p.png'),
         width = 4,
         height = 9)

ggplot(data = filter(cfe_gen_allcase_all100,
                     grepl('^5%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x= TechSensitivity,
               y = AnnualOutput/1e6,
               fill=Fuel),
           colour="black", size= 0.1, width= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.05),
             colour="black", size= .5) +
  geom_text(aes(x = 0.85, y = ci_load_before_time_reduction*.05+nudge_5p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('')+
  ylab('Generation Output (TWh)') +
  coord_cartesian(ylim = output_limit_5p)+
  scale_y_continuous(breaks = output_break_5p) +
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_annual100_5p.png'),
         width = 4,
         height = 9)

ggplot(data = filter(cfe_gen_allcase_all100,
                     grepl('^25%',Scenario),
                     !grepl('CES|Hi.|Ex.|45Q',Scenario)))+
  geom_col(aes(x= TechSensitivity,
               y = AnnualOutput/1e6,
               fill=Fuel),
           colour="black", size= 0.1, width= 0.1) +
  geom_hline(aes(yintercept = ci_load_before_time_reduction*.25),
             colour="black", size= .5) +
  geom_text(aes(x = 0.85, y = ci_load_before_time_reduction*.25+nudge_25p, label = 'Annual Demand'))+
  facet_wrap(Scenario~.,ncol = 1) +
  scale_fill_manual(name = "Resources", values = fuel_colors) +
  xlab('')+
  ylab('Generation Output (TWh)') +
  coord_cartesian(ylim = output_limit_25p)+
  scale_y_continuous(breaks = output_break_25p) +
  geom_hline(yintercept = 0, color = 'grey30')+
  theme_bw() +
  theme(legend.position = 'bottom')+
  ggsave(paste0(temp_RunFdr,'/CompiledResults/',subreg,'/Graphics/CFE_Output_annual100_25p.png'),
         width = 4,
         height = 9)

