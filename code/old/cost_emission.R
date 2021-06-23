source('./code/Header.R')
dir.create(paste0(RunFdr,"/Graphics/cost_emissions"), showWarnings = FALSE)

Cost_csv <- read_csv(paste0(RunFdr,"/CompiledResults/costs.csv"))

total_load <- read_csv(paste0(RunFdr,"/CompiledResults/load_sums_weighted.csv"));
total_load$year <- factor(total_load$year, levels = years)
cost <- subset(Cost_csv, (Costs=="cTotal"), select=c(Costs, Total, case, year))

#sunk costs
generators_csv <- read_csv(paste0(RunFdr,"/CompiledResults/generators.csv"))
generators_costs <- select( generators_csv, region, Resource,zone, cluster, Inv_cost_per_MWyr, Inv_cost_per_MWhyr, case, year)

capacity_csv <- read_csv(paste0(RunFdr,"/CompiledResults/capacity.csv"))
capacity <- select(capacity_csv, Region, Resource, Zone, Cluster, NewCap, NewEnergyCap, case, year) %>%
  filter(Resource != "n/a",year != settings$start_year[1])#it was total but total is no longer  there
capacity$Zone <- as.numeric(capacity$Zone);
capacity$Cluster <- as.numeric(capacity$Cluster);
joined_capacity_and_cost <- merge(capacity, generators_costs,by.x = c("Region", "Resource","Zone","Cluster", "case","year"),by.y = c("region", "Resource","zone","cluster", "case","year")) %>%
  #filter(year != 2050) %>%
  mutate(added_build_cost = Inv_cost_per_MWyr * NewCap) %>%
  mutate(added_energy_cost = Inv_cost_per_MWhyr * NewEnergyCap)

totals <- tibble(case = NULL, year = NULL)
if (years[1] == 2022){
  for(i in 1:length(cases)) {
    zero_row <- tibble(case = cases[i], year = 2022, added = 0)
    # adding 2030 exp cost to 2040
    first_year <- filter(joined_capacity_and_cost, case == cases[i] & year == 2022)
    first_year_total <- sum(first_year$added_build_cost) + sum(first_year$added_energy_cost)
    first_row <- tibble(case = cases[i], year = 2025, added = first_year_total)
    # adding 2040 exp cost to 2050
    second_year <- filter(joined_capacity_and_cost, case == cases[i] & year == 2025)
    second_year_total <- sum(second_year$added_build_cost) + sum(second_year$added_energy_cost)
    second_row <- tibble(case = cases[i], year = 2030, added = second_year_total)
    # adding 2030 exp cost to 2050
    third_year <- filter(joined_capacity_and_cost, case == cases[i] & year == 2022)
    third_year_total <- sum(third_year$added_build_cost) + sum(third_year$added_energy_cost)
    third_row <- tibble(case = cases[i], year = 2030, added = third_year_total)  
    totals <- rbind(totals, zero_row, first_row, second_row, third_row)
  }
} else if (years[1] == 2030){
  for(i in 1:length(cases)) {
    zero_row <- tibble(case = cases[i], year = 2030, added = 0)
    # adding 2030 exp cost to 2040
    first_year <- filter(joined_capacity_and_cost, case == cases[i] & year == 2030)
    first_year_total <- sum(first_year$added_build_cost) + sum(first_year$added_energy_cost)
    first_row <- tibble(case = cases[i], year = 2040, added = first_year_total)
    # adding 2040 exp cost to 2050
    second_year <- filter(joined_capacity_and_cost, case == cases[i] & year == 2040)
    second_year_total <- sum(second_year$added_build_cost) + sum(second_year$added_energy_cost)
    second_row <- tibble(case = cases[i], year = 2050, added = second_year_total)
    # adding 2030 exp cost to 2050
    third_year <- filter(joined_capacity_and_cost, case == cases[i] & year == 2030)
    third_year_total <- sum(third_year$added_build_cost) + sum(third_year$added_energy_cost)
    third_row <- tibble(case = cases[i], year = 2050, added = third_year_total)  
    #obtain battery cost from 2030 because it has less than a 30 year life span
    battery_old_cost <- filter(joined_capacity_and_cost, case == cases[i] & year == 2030 & Resource == "battery_mid")
    battery_old_cost_total <- sum(battery_old_cost$added_build_cost) + sum(battery_old_cost$added_energy_cost)
    battery_old_cost_row <- tibble(case = cases[i], year = 2050, added = battery_old_cost_total * -1)
    totals <- rbind(totals, zero_row, first_row, second_row, third_row, battery_old_cost_row)
  }
}




totals <- aggregate(added~case+year, totals, sum)
  
totals <- group_by(totals, case) %>%
  mutate(csum = cumsum(added)) %>%
  ungroup() 


cost <- full_join(cost, totals) %>%
  mutate(Total = Total + csum)

# transmission
transmission_csv <- read_csv(paste0(RunFdr,"/CompiledResults/trans.csv"))
transmission <- mutate(transmission_csv, 
                       Cost_Trans_Capacity = ifelse(Cost_Trans_Capacity < 0, 0, Cost_Trans_Capacity))
transmission <- aggregate(Cost_Trans_Capacity~case+year, transmission, sum)

totals <- tibble(case = NULL, year = NULL)
if (years[1] == 2022) {
  for(i in 1:length(cases)) {
    zero_row <- tibble(case = cases[i], year = 2022, added = 0)
    # adding 2030 exp cost to 2040
    first_year <- filter(transmission, case == cases[i] & year == 2022)
    first_year_total <- sum(first_year$Cost_Trans_Capacity)
    first_row <- tibble(case = cases[i], year = 2025, added = first_year_total)
    # adding 2040 exp cost to 2050
    second_year <- filter(transmission, case == cases[i] & year == 2025)
    second_year_total <- sum(second_year$Cost_Trans_Capacity)
    second_row <- tibble(case = cases[i], year = 2030, added = second_year_total)
    # adding 2030 exp cost to 2050
    third_year <- filter(transmission, case == cases[i] & year == 2022)
    third_year_total <- sum(third_year$Cost_Trans_Capacity)
    third_row <- tibble(case = cases[i], year = 2030, added = third_year_total) 
    totals <- rbind(totals, zero_row, first_row, second_row, third_row)
  }
  } else if (years[1] == 2030){
    for(i in 1:length(cases)) {
      zero_row <- tibble(case = cases[i], year = 2030, added = 0)
      # adding 2030 exp cost to 2040
      first_year <- filter(transmission, case == cases[i] & year == 2030)
      first_year_total <- sum(first_year$Cost_Trans_Capacity)
      first_row <- tibble(case = cases[i], year = 2040, added = first_year_total)
      # adding 2040 exp cost to 2050
      second_year <- filter(transmission, case == cases[i] & year == 2040)
      second_year_total <- sum(second_year$Cost_Trans_Capacity)
      second_row <- tibble(case = cases[i], year = 2050, added = second_year_total)
      # adding 2030 exp cost to 2050
      third_year <- filter(transmission, case == cases[i] & year == 2030)
      third_year_total <- sum(third_year$Cost_Trans_Capacity)
      third_row <- tibble(case = cases[i], year = 2050, added = third_year_total) 
      totals <- rbind(totals, zero_row, first_row, second_row, third_row)
    }
}


totals <- aggregate(added~case+year, totals, sum)

totals <- group_by(totals, case) %>%
  mutate(csumT = cumsum(added)) %>%
  ungroup() 

cost <- merge(cost, totals,by=c("case","year")) %>%
  mutate(Total = Total + csumT)

cost$year <- as.factor(cost$year);
cost <- full_join(cost, total_load) %>%
  mutate(`$/MWh` = Total/systemload)
cost$case <- factor(cost$case, levels = cases)
ggplot(cost , aes(x=case, y=`$/MWh`, fill = case))+
    geom_bar(stat="identity",width = 0.3) + 
    theme_bw()+ 
  facet_wrap(~year) +
    theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
    labs(x="cases", y="$/MWh") +
  ggsave(paste0(RunFdr,"/Graphics/cost_emissions/costs_all.png"), width=10, height=5, dpi=300)

# plotting the emissions and cost--------

emissions_csv <- read_csv(paste0(RunFdr,"/CompiledResults/CO2.csv"))
emissions <- emissions_csv %>%
  filter(Zone=="Sum") %>%
  select(-c(Total,Zone)) %>%
  pivot_longer(c(1:NoZone)) %>%
  group_by(case, year) %>%
  summarize(Total = sum(value))
emissions$year <- as.factor(emissions$year);
emissions <- left_join(emissions, total_load) %>%
  mutate(`CO2tons/MWh` = Total/systemload)

emissions$case <- factor(emissions$case, levels = cases)




ggplot(emissions , aes(x=case, y=`CO2tons/MWh`, fill = case))+
  geom_bar(stat="identity",width = 0.3) + 
  theme_bw()+ 
  facet_wrap(~year) +
  theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
  labs(x="cases", y="CO2tons/MWh")+
  ggsave(paste0(RunFdr,"/Graphics/cost_emissions/emissions_all.png"), width=10, height=5, dpi=300)

cost <- cost  %>%
  rename(Cost = Total)
emissions <- emissions %>%
  rename(Emission = Total)
joined <- full_join(cost, emissions,by = c("case","year"))
joined$year <- as.factor(joined$year)
ggplot(joined, aes(x = `CO2tons/MWh`, y = `$/MWh`, color = case, shape = year)) + geom_point(size = 3) +
  labs(x="CO2tons/MWh", y="$/MWh")+
  theme_bw()+
  ggsave(paste0(RunFdr,"/Graphics/cost_emissions/cost_by_emissions.png"), width=10, height=5, dpi=300)


ggplot(emissions , aes(x=case, y=`Emission`, fill = case))+
  geom_bar(stat="identity",width = 0.3) + 
  theme_bw()+ 
  facet_wrap(~year) +
  theme(text = element_text(size=8), legend.key.size = unit(0.5, "cm"))+
  labs(x="cases", y="Total Emission CO2e Metric Tons")+
  ggsave(paste0(RunFdr,"/Graphics/cost_emissions/emissions_all_mass.png"), width=10, height=5, dpi=300)


ggplot(joined, aes(x = `Emission`/1e6, y = `$/MWh`, color = case, shape = year)) + geom_point(size = 3) +
  labs(x="Total Emission CO2e Million Metric Tons", y="$/MWh")+
  theme_bw()+
  ggsave(paste0(RunFdr,"/Graphics/cost_emissions/cost_by_emissions_mass.png"), width=10, height=5, dpi=300)

