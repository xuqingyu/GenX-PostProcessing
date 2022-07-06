#post processing
options(readr.show_progress = FALSE)

# the naming convention of PG is
# '[...]/[Running_folder]/[year]/[case_id]_[year]_[case_description]/Results/'
# so it is in fact preferable to feed in the setup folder;
RunFdr <- "/Users/qingyuxu/Documents/WECC_247_trade_ccs100_p500_new//"
# RunFdr <- "/tigress/qingyux/GenX/PJM/2022_PJM_CE/pjm_ce_all/"
settingfile <- 'postprocessing_inputs.csv';

source('./code/Header.R')

# Level 1: Compiling ----
source('./code/level_1_compiling/compile_model_cost.R') #Done
source('./code/level_1_compiling/compile_model_zonal_cost.R') #Done
source('./code/level_1_compiling/compile_lse_nse_cost.R') #Done
source('./code/level_1_compiling/compile_energy_demand.R') # Done
source('./code/level_1_compiling/compile_nonserved_energy.R')
source('./code/level_1_compiling/compile_capacity.R') #Done
source('./code/level_1_compiling/compile_power_output.R') #Done
source('./code/level_1_compiling/compile_power_charge.R') #Done
source('./code/level_1_compiling/compile_power_curtailment.R') #Done
# source('./code/level_1_compiling/compile_capacity_value.R') #Done, but very time consuming, probably because the table is too wide
source('./code/level_1_compiling/compile_transmission.R') #Done
source('./code/level_1_compiling/compile_zonal_transmission_loss.R')
source('./code/level_1_compiling/compile_transmission_congestion_revenue.R') # Done
source('./code/level_1_compiling/compile_lse_transmission_loss_payment.R') # Done
source('./code/level_1_compiling/compile_generation_settlement.R') #Done
source('./code/level_1_compiling/compile_energy_price.R') #Done
source('./code/level_1_compiling/compile_capacity_price.R') #Done
# source('./code/level_1_compiling/compile_generation_data.R') # Currently not supported
source('./code/level_1_compiling/compile_carbon_emissions.R') #Done
source('./code/level_1_compiling/compile_carbon_cap_massbased.R') #Done
source('./code/level_1_compiling/compile_carbon_cap_massbased_price.R') #Done
source('./code/level_1_compiling/compile_carbon_cap_massbased_lse_revenue.R') #Done
source('./code/level_1_compiling/compile_carbon_cap_loadbased.R') # Done
source('./code/level_1_compiling/compile_carbon_cap_loadbased_price.R') # Currently not supported
source('./code/level_1_compiling/compile_carbon_cap_loadbased_lse_revenue.R') # Currently not supported
# source('./code/level_1_compiling/compile_carbon_cap_genbased.R') # Currently not supported
# source('./code/level_1_compiling/compile_carbon_cap_genbased_price.R') # Currently not supported
source('./code/level_1_compiling/compile_carbon_tax_cost_n_revenue.R') # Done
source('./code/level_1_compiling/compile_renewable_energy_requirement.R') #Done
source('./code/level_1_compiling/compile_renewable_energy_price.R') # Done
source('./code/level_1_compiling/compile_energy_lse_payment.R') # Done
source('./code/level_1_compiling/compile_capacity_lse_payment.R') # Done
source('./code/level_1_compiling/compile_capacity_trans_revenue.R') # Done
source('./code/level_1_compiling/compile_renewable_energy_lse_payment.R')
source('./code/level_1_compiling/compile_renewable_energy_transmissionloss_payment.R')
# Level 2: Result Calculation ----
source('./code/level_2_calculation/stitch_lse_payment_wo_transmission_allocation.R') # Done
source('./code/level_2_calculation/transmission_congestion_n_cost_for_allocation.R') # Done
source('./code/level_2_calculation/transmission_cost_allocation.R') # Done
# we don't do transmission capacity revenue allocation since 
# they are supposed to be earned by outside generations


# Level 3: Polishing ----
# Subregional Aggregation and add case name 
source('./code/level_3_polishing/energy_price_add_scenario_techsensitivity_name.R') #Done
source('./code/level_3_polishing/energy_price_timeseries.R') #Done
source('./code/level_3_polishing/capacity_price_add_scenario_techsensitivity_name.R') #Done
source('./code/level_3_polishing/capacity_price_timeseries.R') #Done
source('./code/level_3_polishing/subregion_aggregate_gen_profit.R') # #Done
source('./code/level_3_polishing/subregion_aggregate_gen_capacity.R') #Done
# source('./code/level_3_polishing/subregion_aggregate_gen_capacity_value.R') #Done
source('./code/level_3_polishing/subregion_aggregate_gen_misc.R') #Done
source('./code/level_3_polishing/subregion_aggregate_gen_output.R') #Done
# source('./code/level_3_polishing/subregion_aggregate_gen_output_timeseries.R') # time consuming
source('./code/level_3_polishing/subregion_aggregate_gen_curtailment.R') #Done
source('./code/level_3_polishing/subregion_aggregate_gen_clean_output.R') #Done
source('./code/level_3_polishing/subregion_aggregate_storage_operation.R') #Done
source('./code/level_3_polishing/subregion_aggregate_load_component.R') # This is currently tailored for PJM.
# source('./code/level_3_polishing/subregion_aggregate_load_timeseries.R') # This will use gen_output time-series because flexible load is used.
source('./code/level_3_polishing/subregion_aggregate_lse_payment.R') # Currently not supported, missing file
source('./code/level_3_polishing/subregion_aggregate_system_cost.R') # Currently not supported, missing file
source('./code/level_3_polishing/subregion_aggregate_emissions.R') # Not fully supported
source('./code/level_3_polishing/subregion_aggregate_emission_v_systemcost.R')
# source('./code/level_3_polishing/subregion_aggregate_renewable_market.R') # Currently not supported, missing file
source('./code/level_3_polishing/subregion_aggregate_netimport.R')

# PJM study special 
# source('./code/misc/Trans_Expansion.R')
