
# Setting Up Packages -------------------------------------------------
library(tidyverse)
library(ggpubr)
# library(cowplot)
# Actively Scanning Result Folders ---------------------------------------

dir.create(paste0(RunFdr,"/CompiledResults/"), showWarnings = FALSE)
settings <- read_csv(paste0(RunFdr,"/",settingfile), col_types = cols());
resource_mapping <- select(settings, Resource, Fuel);
resource_list <- unique(settings$Fuel);
zone_mapping <- na.omit(select(settings, zone, region)) 
zone_mapping$zone = as.factor(zone_mapping$zone)
zone_count <- na.omit(settings$zone_count)
# This read how transmission cost will be allocated
trans_cost_mapping <- na.omit(select(settings, `Network_lines`, `Transmission Path Name`,`SystemOperatorName`,`SystemOperatorShare`))
trans_line_name <- na.omit(select(settings, `Network_lines`, `Transmission Path Name`))
trans_cost_mapping$`Network_lines` <- as.factor(trans_cost_mapping$`Network_lines`);
trans_cost_startcost <- na.omit(select(settings,`SystemOperator`,`ExistingTransmissionCost`))
years_pre <- na.omit(select(settings,`Model_years`,`Pre_years`))
years_pre$Model_years <- as.factor(years_pre$Model_years)
years_pre$Pre_years <- as.factor(years_pre$Pre_years)
rto_mapping <- na.omit(select(settings, zone, region,System_membership)) %>%
  mutate(loadzone = paste('Load_MW_z',zone,sep=""))
rto_mapping$zone <- factor(rto_mapping$zone, levels = zone_mapping$zone)
Interested_Regions <- as.character(na.omit(settings$Interested_Regions))
Deep_Dive <- as.character(na.omit(settings$Deep_Dive))
Subregions <- na.omit(unique(settings$Subregions))
n_subregions <- length(Subregions);
Subregion_zones <- na.omit(unique(select(settings, Subregions, Subregion_zones)))
case_ids <- as.character(na.omit(settings$case_id))
cases <- as.character(na.omit(settings$case_description))
years <- as.character(na.omit(settings$list_years))
cases_newnames <- na.omit(select(settings,case_description,`Scenario`,`TechSensitivity`))
scenario <- c(na.omit(unique(settings$Scenario)))
tech_sensitivity <-  c(na.omit(unique(settings$TechSensitivity)))

comparison <- c(na.omit(unique(settings$Scenario_Comparison)))
n_comparison <- length(comparison);
compared_scenario <- na.omit(select(settings,Scenario_Comparison,Compared_Scenario))
interested_sensitivity <- c(na.omit(unique(settings$Interested_Sensitivity)))

load_mapping <- na.omit(select(settings,case_description,`SCENARIO_Load`))

colors <- select(settings,Capacity_Fuel,Color) %>% distinct() %>% na.omit() %>% rename(Fuel = Capacity_Fuel)
fuel_colors <- colors %>% pivot_wider(names_from = Fuel, values_from=Color) 
capacity_resource_colors <- as.character(colors$Color) %>% na.omit()
capacity_resource_levels <- as.character(colors$Fuel) %>% na.omit()
fuel_list <- as.character(na.omit(settings$Power_Fuel));
storage_fuel <- as.character(na.omit(settings$Storage_Fuel));
flexiload_list <- na.omit(unique(settings$FlexibleLoad))
power_colors <- filter(colors, Fuel %in% fuel_list);

color_list <- as.character(power_colors$Color);
dir.create(paste0(RunFdr,"/Graphics"), showWarnings = FALSE)
dir.create(paste0(RunFdr,"/Graphics/EnergyPrice"), showWarnings = FALSE)
for (i in 1:n_subregions){
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i]), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Generation"), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Load"), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Emissions"), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Graphics"), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Graphics/GenOutput"), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Graphics/GenCapacity"), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Graphics/SystemCost"), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Graphics/LSECost"), showWarnings = FALSE)
  dir.create(paste0(RunFdr,"/CompiledResults/",Subregions[i],"/Graphics/GenProfit"), showWarnings = FALSE)
}

load_cost_color <- select(settings,Load_Cost_Type,Load_Cost_Color) %>% distinct() %>% na.omit() %>% pivot_wider(names_from = Load_Cost_Type, values_from=Load_Cost_Color) 
load_cost_type <- colnames(load_cost_color)
system_cost_color <- select(settings,System_Cost_Type,System_Cost_Color) %>% distinct() %>% na.omit() %>% pivot_wider(names_from = System_Cost_Type, values_from=System_Cost_Color) 
system_cost_type <- colnames(system_cost_color)
genprofit_color <- select(settings,GenProfit_Type,GenProfit_Color) %>% distinct() %>% na.omit() %>% pivot_wider(names_from = GenProfit_Type, values_from=GenProfit_Color) 
genprofit_type <- colnames(genprofit_color)

sensitivity_comparison <- unique(na.omit(settings$TechSensitivity_Comparison))
n_sensitivity_comparison <- length(sensitivity_comparison)
sensitivity_comparison_target <- select(settings,TechSensitivity_Comparison, TechSensitivity_Comparison_Target) %>% na.omit() %>% unique()
sensitivity_comparison_sensitivity <- select(settings,TechSensitivity_Comparison, TechSensitivity_Comparison_Sensitivity) %>% na.omit() %>% unique()
interested_scenario <- c(na.omit(unique(settings$Focused_Scenario)))

TS_cases<- settings$TS_cases %>% na.omit()
TS_cases_id <- settings$TS_cases_id %>% na.omit()
renewable_fuel <- settings$Renewable_Fuel %>% na.omit()
clean_fuel <- settings$Clean_Fuel %>% na.omit()

interface <- unique(na.omit(settings$Interface))
n_interface <- length(interface)
interface_line_mapping <- select(settings, Interface, Interface_Line, Interface_Line_Direction) %>% 
  na.omit() %>%
  mutate(Interface_Line = as.character(Interface_Line))



