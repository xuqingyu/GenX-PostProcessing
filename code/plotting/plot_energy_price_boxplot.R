energyprice_ts <- read_csv(paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries.csv')) %>%
  left_join(cases_newnames,by=c('case' = 'case_description')) %>%
  mutate(Scenario = factor(Scenario, levels = scenario),
         TechSensitivity = factor(TechSensitivity, levels = tech_sensitivity),
         year = factor(year, levels = years))
  
energyprice_2019_summary <- read_csv('./data/2019_energy_price_summary.csv')

for (j in 1:n_comparison){
  for (i in 1:length(Interested_Regions)){
    for (k in 1:length(interested_sensitivity)){
      temp_compared_scenario <- compared_scenario$Compared_Scenario[compared_scenario$Scenario_Comparison == comparison[j]]
      temp_plot <- energyprice_ts  %>%
        filter(Scenario %in% temp_compared_scenario,
               Region == Interested_Regions[i],
               TechSensitivity == interested_sensitivity[k])
      temp_plot_limit <- temp_plot %>%
        group_by(Scenario, year) %>%
        summarise(limit = quantile(Price,0.95))
      temp_plot_limit <- max(temp_plot_limit$limit)
      quantile(temp_plot$Price,0.99)
      bar <- energyprice_2019_summary$AnnualPrice[energyprice_2019_summary$name == Interested_Regions[i]]
      ggplot(data=temp_plot,aes(x=year, y=Price)) +
        geom_boxplot(outlier.shape = NA,coef=0) +
        geom_hline(yintercept = bar) + 
        stat_summary(fun=mean, geom="point", shape=20, size=2, color="red", fill="red") +
        theme_bw()+
        coord_cartesian(ylim = c(0,100)) +
        facet_grid(.~Scenario)+
        labs(caption = 'Red dot shows the annual-average energy price\n Horizontal line shows the 2019 Annual Average Energy Price (simulated)')+
        ggtitle(paste0('Energy Price distribution of Region ', Interested_Regions[i], ' Under \nSensitivity ',interested_sensitivity[k]))+
        ylab("Energy Price ($/MWh)") + 
        ggsave(paste0(RunFdr,'/Graphics/EnergyPrice/Energy Price of Region ',Interested_Regions[i],"_",comparison[j],'_',k,'.png'),
               width = 10,
               height=7)
    }
  }
}

# temp_plot <- energyprice_ts  %>%
#   filter(case == 'deepdecarbonization_mid', year == 2050, grepl('PJM_NJ*',Region)) %>%
#   filter(HourID %in% c(1:672))
# ggplot(data=temp_plot,aes(x=HourID, y=Price, color=Region)) +
#   geom_line() +
#   coord_cartesian(ylim = c(0,200)) +
#   geom_vline(xintercept = seq(from=1, to = 672, by = 24),color = 'grey90') +
#   theme_classic()+
#   ylab("Energy Price ($/MWh)")


# 
# energyprice_reduced <- read_csv(paste0(RunFdr,'/CompiledResults/EnergyPrice_timeseries_reduced.csv'))
# energyprice_reduced_statistics <- energyprice_reduced %>%
#   mutate(cluster = ceiling(Time_index/168)) %>%
#   group_by(Region, case,year,cluster) %>%
#   summarize(`Avg.Price` = mean(Price))
# temp_plot <- energyprice_reduced_statistics  %>%
#   filter(grepl('PJM_NJ*',Region),grepl('statedpolicy_',case),grepl('_mid',case))
# ggplot(data=temp_plot,aes(x=cluster, y=Avg.Price, color=Region)) +
#   geom_point() +
#   coord_cartesian(ylim = c(0,100)) +
#   geom_vline(xintercept = c(1:18),color="grey90")+
#   theme_classic()+
#   facet_grid(year~case)+
#   ylab("Avg.Price ($/MWh)")
