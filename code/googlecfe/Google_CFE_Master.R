# Google CFE master file
library(tidyverse)

Studyregion = 'WECC' #WECC
n_cfe = 1
{
if(Studyregion == 'PJM'){
  subreg = 'PJM_Google'
  # temp_RunFdr = "/Users/qingyuxu/Documents/PJM_Google_12x7/"
  temp_RunFdr = "/Users/qingyuxu/Documents/PJM_Google_12x7_Anticipated/"
  temp_RunFdr = "/Users/qingyuxu/Documents/PJM_Google_12x7_newruns/"
  temp_RunFdr = "/Users/qingyuxu/Documents/PJM_Google_12x7_newruns_it2/"
  reference_fn = "/Users/qingyuxu/Dropbox (Princeton)/PJM/Project_GenX_OutputProcessing/data/Google_PJM_CFEStudy_Reference.csv"
} else {
  subreg = 'California'
  temp_RunFdr = "/Users/qingyuxu/Documents/WECC_Google_Anticipated//"
  temp_RunFdr = "/Users/qingyuxu/Documents/WECC_Google_newruns_it2//"
  reference_fn = "/Users/qingyuxu/Dropbox (Princeton)/PJM/Project_GenX_OutputProcessing/data/Google_WECC_CFEStudy_Reference.csv"
}
reference = read_csv(reference_fn)
p_width = 12
p_height = 7
Scenario_filter = '10%'
x_list = c("No 24x7 Purchase", "Hourly 80%",
           "Hourly 82%","Hourly 84%","Hourly 86%","Hourly 88%","Hourly 90%",
           "Hourly 92%","Hourly 94%","Hourly 96%","Hourly 98%","Hourly 100%","Annual 100%")

y_list = c(
  '5% CI Part., Curt. Tech.',
  '10% CI Part., Curt. Tech.',
  '25% CI Part., Curt. Tech.',
  '5% CI Part., Adv. Tech. no Comb.',
  '10% CI Part., Adv. Tech. no Comb.',
  '25% CI Part., Adv. Tech. no Comb.',
  '5% CI Part., Adv. Tech. Full',
  '10% CI Part., Adv. Tech. Full',
  '25% CI Part., Adv. Tech. Full',
  '10% CI Part., Curt. Tech., 80% CES',
  '10% CI Part., Adv. Tech. no Comb., 80% CES',
  '10% CI Part., Adv. Tech. Full, 80% CES',
  '10% CI Part., Adv. Tech. Full, no 45Q',
  "10% CI Part., Curt. Tech., no Ex. Limit",
  '10% CI Part., Adv. Tech. no Comb., no Ex. Limit',
  "10% CI Part., Adv. Tech. Full, no Ex. Limit",
  "10% CI Part., Curt. Tech., Hi. Nat. Gas P.",
  "10% CI Part., Adv. Tech. no Comb., Hi. Nat. Gas P.",
  '10% CI Part., Adv. Tech. Full, Hi. Nat. Gas P.'
  )


# Read settings ----
settingfile <- 'sample_inputs_pjm.csv';
settings <- read_csv(paste0(temp_RunFdr,"/",settingfile));
resource_mapping <- select(settings, Resource, Fuel);
resource_list <- unique(settings$Fuel);
flexiload_list <- na.omit(unique(settings$FlexibleLoad))
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
power_colors <- filter(colors, Fuel %in% fuel_list);
color_list <- as.character(power_colors$Color);
dir.create(paste0(temp_RunFdr,"/CompiledResults/"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/Graphics"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/Graphics/EnergyPrice"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Generation"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Load"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Emissions"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Graphics"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Graphics/GenOutput"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Graphics/GenCapacity"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Graphics/SystemCost"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Graphics/LSECost"), showWarnings = FALSE)
dir.create(paste0(temp_RunFdr,"/CompiledResults/",subreg,"/Graphics/GenProfit"), showWarnings = FALSE)

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

resource_mapping_includingflexibleload <- select(settings, All_Resource, All_Fuel) %>% na.omit() %>% distinct()
TS_cases<- settings$TS_cases %>% na.omit()
TS_cases_id <- settings$TS_cases_id %>% na.omit()
renewable_fuel <- settings$Renewable_Fuel %>% na.omit()
clean_fuel <- settings$Clean_Fuel %>% na.omit()

interface <- unique(na.omit(settings$Interface))
n_interface <- length(interface)
interface_line_mapping <- select(settings, Interface, Interface_Line, 
                                 Interface_Line_Direction) %>% na.omit() %>%
  mutate(Interface_Line = as.character(Interface_Line))
}
# Run code ----

source('./code/googlecfe/Google_CFE_capacity_output_and_curtail.R')
# source('./code/googlecfe/Google_CFE_Emission.R')
# source('./code/googlecfe/Google_CFE_Score_and_TS.R')
source('./code/googlecfe/Google_CFE_score_ts_emission.R')
source('./code/googlecfe/Google_CFE_LSEPayment.R')
source('./code/googlecfe/Google_CFE_Plot.R')
