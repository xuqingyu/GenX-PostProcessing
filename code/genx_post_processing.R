#post processing
options(readr.show_progress = FALSE)
source('./code/Header.R')
# Level 1: Compiling ----
source('./code/level_1_compiling/compile_model_cost.R')
source('./code/level_1_compiling/compile_model_zonal_cost.R')
source('./code/level_1_compiling/compile_lse_nse_cost.R')
source('./code/level_1_compiling/compile_energy_demand.R')
source('./code/level_1_compiling/compile_capacity.R')
source('./code/level_1_compiling/compile_power_output.R')
source('./code/level_1_compiling/compile_power_charge.R')
source('./code/level_1_compiling/compile_power_curtailment.R')
source('./code/level_1_compiling/compile_capacity_value.R')
source('./code/level_1_compiling/compile_transmission.R')
source('./code/level_1_compiling/compile_transmission_congestion_revenue.R')
source('./code/level_1_compiling/compile_lse_transmission_loss_payment.R')
source('./code/level_1_compiling/compile_generation_settlement.R')
source('./code/level_1_compiling/compile_energy_price.R')
source('./code/level_1_compiling/compile_capacity_price.R')
source('./code/level_1_compiling/compile_generation_data.R')
source('./code/level_1_compiling/compile_carbon_emissions.R')
source('./code/level_1_compiling/compile_carbon_cap_massbased.R')
source('./code/level_1_compiling/compile_carbon_cap_massbased_price.R')
source('./code/level_1_compiling/compile_carbon_cap_massbased_lse_revenue.R')
source('./code/level_1_compiling/compile_carbon_cap_loadbased.R')
source('./code/level_1_compiling/compile_carbon_cap_loadbased_price.R')
source('./code/level_1_compiling/compile_carbon_cap_loadbased_lse_revenue.R')
source('./code/level_1_compiling/compile_carbon_cap_genbased.R')
source('./code/level_1_compiling/compile_carbon_cap_genbased_price.R')
source('./code/level_1_compiling/compile_carbon_tax_cost_n_revenue.R')
source('./code/level_1_compiling/compile_renewable_energy_requirement.R')
source('./code/level_1_compiling/compile_renewable_energy_price.R')
source('./code/level_1_compiling/compile_energy_lse_payment.R')
source('./code/level_1_compiling/compile_capacity_lse_payment.R')
source('./code/level_1_compiling/compile_renewable_energy_lse_payment.R')

# Level 2: Result Calculation ----
source('./code/level_2_calculation/stitch_lse_payment_wo_transmission_allocation.R')
source('./code/level_2_calculation/transmission_congestion_n_cost_for_allocation.R')
source('./code/level_2_calculation/transmission_cost_allocation.R')


# Level 3: Polishing ----
# Subregional Aggregation and add case name 
source('./code/level_3_polishing/energy_price_add_scenario_techsensitivity_name.R')
source('./code/level_3_polishing/energy_price_timeseries.R')
source('./code/level_3_polishing/capacity_price_add_scenario_techsensitivity_name.R')
source('./code/level_3_polishing/capacity_price_timeseries.R')
source('./code/level_3_polishing/subregion_aggregate_gen_profit.R')
source('./code/level_3_polishing/subregion_aggregate_gen_capacity.R')
source('./code/level_3_polishing/subregion_aggregate_gen_capacity_value.R')
source('./code/level_3_polishing/subregion_aggregate_gen_misc.R')
source('./code/level_3_polishing/subregion_aggregate_gen_output.R')
source('./code/level_3_polishing/subregion_aggregate_gen_output_timeseries.R')
source('./code/level_3_polishing/subregion_aggregate_gen_clean_output.R')
source('./code/level_3_polishing/subregion_aggregate_load_component.R') # This is currently tailored for PJM.
source('./code/level_3_polishing/subregion_aggregate_load_timeseries.R') # This will use gen_output time-series because flexible load is used.
source('./code/level_3_polishing/subregion_aggregate_lse_payment.R')
source('./code/level_3_polishing/subregion_aggregate_system_cost.R')
source('./code/level_3_polishing/subregion_aggregate_emissions.R')
source('./code/level_3_polishing/subregion_aggregate_emission_v_systemcost.R')
source('./code/level_3_polishing/subregion_aggregate_renewable_market.R')


# PJM study special 
source('./code/misc/Trans_Expansion.R')
