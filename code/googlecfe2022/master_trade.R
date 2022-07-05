library(tidyverse)

Studyregion = 'WECC' #WECC
natozero <- function(x) {x[which(is.na(x))] <-0; return(x)}
negtozero <- function(x) {x[x<=0] <-0; return(x)}
countercapres <- function(x) {x[x==0] <- (-1); return(x)}
n_tfs = 5
cistudy = 0

settingfile <- 'postprocessing_inputs.csv';


if(Studyregion == 'PJM'){
  subreg = 'PJM_Google'
  RunFdr <- "/Users/qingyuxu/Documents/PJM_247/";
  if (n_tfs == 1 & cistudy == 1) {
    cfe_load_ori <- read_csv(paste0(RunFdr,'PJM_RPSH_Load_data_1.csv'), 
                             col_types = cols())
  }
} else {
  subreg = 'California'
  RunFdr <- "/Users/qingyuxu/Documents/WECC_247_trade_ccs100_p500_new//";
  if (n_tfs == 1 & cistudy == 1) {
    cfe_load_ori <- read_csv(paste0(RunFdr,'WECC_RPSH_Load_data_1.csv'), 
                             col_types = cols())
  }
}

source('./code/Header.R')
p_width = 12
p_height = 7
# Scenario_filter = '10%'
# x_list = c("No 24x7 Purchase", "Hourly 80%",
#            "Hourly 82%","Hourly 84%","Hourly 86%","Hourly 88%","Hourly 90%",
#            "Hourly 92%","Hourly 94%","Hourly 96%","Hourly 98%","Hourly 100%","Annual 100%")
# 
# y_list = c(
#   '5% CI Part., Curt. Tech.',
#   '10% CI Part., Curt. Tech.',
#   '25% CI Part., Curt. Tech.',
#   '5% CI Part., Adv. Tech. no Comb.',
#   '10% CI Part., Adv. Tech. no Comb.',
#   '25% CI Part., Adv. Tech. no Comb.',
#   '5% CI Part., Adv. Tech. Full',
#   '10% CI Part., Adv. Tech. Full',
#   '25% CI Part., Adv. Tech. Full',
#   '10% CI Part., Curt. Tech., 80% CES',
#   '10% CI Part., Adv. Tech. no Comb., 80% CES',
#   '10% CI Part., Adv. Tech. Full, 80% CES',
#   '10% CI Part., Adv. Tech. Full, no 45Q',
#   "10% CI Part., Curt. Tech., no Ex. Limit",
#   '10% CI Part., Adv. Tech. no Comb., no Ex. Limit',
#   "10% CI Part., Adv. Tech. Full, no Ex. Limit",
#   "10% CI Part., Curt. Tech., Hi. Nat. Gas P.",
#   "10% CI Part., Adv. Tech. no Comb., Hi. Nat. Gas P.",
#   '10% CI Part., Adv. Tech. Full, Hi. Nat. Gas P.'
# )

source('./code/googlecfe2022/cfe_table_compile.R')
source('./code/googlecfe2022/cfe_emission_table_compile.R')
source('./code/googlecfe2022/cfe_system_emission_table_compile.R')
source('./code/googlecfe2022/cfe_capacity_compile.R')
source('./code/googlecfe2022/cfe_output_compile.R')
source('./code/googlecfe2022/cfe_curtailment_compile.R')
source('./code/googlecfe2022/cfe_gen_ts.R')
source('./code/googlecfe2022/cfe_modifiedload_compile.R')
source('./code/googlecfe2022/cfe_load_payment_compile.R')
source('./code/googlecfe2022/cfe_netexport_compile.R')
source('./code/googlecfe2022/cfe_importexport_compile.R')
if(Studyregion == 'PJM'){
  subreg = 'PJM_Google'
  if (n_tfs == 1) {
    source('./code/googlecfe2022/cfe_extract_reference_pjm.R')
    reference_fn = "./data/Google_PJM_CFEStudy_Reference_2022.csv"
    reference = read_csv(reference_fn, col_types = cols())
  }
} else {
  subreg = 'California'
  if (n_tfs == 1) {
    reference_fn = "./data/Google_WECC_CFEStudy_Reference_2022.csv"
    source('./code/googlecfe2022/cfe_extract_reference_wecc.R')
    reference = read_csv(reference_fn, col_types = cols())
  }
}

