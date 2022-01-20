zcf_repowercostupdate <- function(Settlement_onerow,repowercost){
  repower_inv <- (-1)*repowercost*Settlement_onerow$EndCap
  Settlement_onerow$Inv_cost_MW = repower_inv
  Settlement_onerow$Inv_cost = repower_inv
  Settlement_onerow$Fixed_OM_cost_MW = Settlement_onerow$Fixed_OM_cost_MW - repower_inv
  Settlement_onerow$FOM <- Settlement_onerow$FOM - repower_inv
  return(Settlement_onerow)
}
for (i in 1:nrow(Settlement)) {
  if (Settlement$year[i] == '2040') {
    # generator.loc[(generator.region == 'NY_East')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 52685 # 50% of new cc capex in the same region
    # generator.loc[(generator.region == 'NY_East')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 36818 # 50% of new ct capex in the same region
    # generator.loc[(generator.region == 'NY_West')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37822 # 50% of new cc capex in the same region
    # generator.loc[(generator.region == 'NY_West')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27489 # 50% of new ct capex in the same region
    if ((Settlement$Region[i] == 'NY_East')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')) {
      repowercost <- 38498
      Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
    } else if ((Settlement$Region[i] == 'NY_East')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
      repowercost <- 26904
      Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
    } else if ((Settlement$Region[i] == 'NY_West')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
      repowercost <- 27637
      Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
    } else if ((Settlement$Region[i] == 'NY_West')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
      repowercost <- 20087
      Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
    }
  }
  if (Settlement$year[i] == '2050'){
    # generator.loc[(generator.region == 'PJM_NJCoast')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37454 # 50% of new cc capex in the same region
    # generator.loc[(generator.region == 'PJM_NJCoast')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27511 # 50% of new ct capex in the same region
    # generator.loc[(generator.region == 'PJM_NJLand')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37454 # 50% of new cc capex in the same region
    # generator.loc[(generator.region == 'PJM_NJLand')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27511 # 50% of new ct capex in the same region
    # generator.loc[(generator.region == 'PJM_Dom')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 34621 # 50% of new cc capex in the same region
    # generator.loc[(generator.region == 'PJM_Dom')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 24957 # 50% of new ct capex in the same region                     
    # 
    if (grepl('statedpolicy',Settlement$case[i])) {
      if ((Settlement$Region[i] %in% c('PJM_NJCoast','PJM_NJLand'))&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')) {
        repowercost <- 27368
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] %in% c('PJM_NJCoast','PJM_NJLand'))&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        repowercost <- 20103
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } 
      # else if ((Settlement$Region[i] == 'PJM_Dom')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
      #   repowercost <- 25298
      #   Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      # } else if ((Settlement$Region[i] == 'PJM_Dom')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
      #   repowercost <- 18237
      #   Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      # }
    } else if (grepl('deepdecarbonization',Settlement$case[i])){
      # generator.loc[(generator.region == 'PJM_Delaware')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37454 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'PJM_Delaware')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27511 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'PJM_Dom')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 34621 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'PJM_Dom')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 24957 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'PJM_NJCoast')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37454 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'PJM_NJCoast')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27511 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'PJM_NJLand')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37454 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'PJM_NJLand')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27511 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'PJM_PECO')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37454 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'PJM_PECO')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27511 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'PJM_SMAC')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37454 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'PJM_SMAC')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27511 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'PJM_WEST')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 30668 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'PJM_WEST')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 23473 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'PJM_WestMAC')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 37454 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'PJM_WestMAC')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 27511 # 50% of new ct capex in the same region 
      # generator.loc[(generator.region == 'MIS_Central')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 33073 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'MIS_Central')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 25751 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'MIS_East')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 33929 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'MIS_East')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 26545 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'SC_TVA')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 29680 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'SC_TVA')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 23231 # 50% of new ct capex in the same region
      # generator.loc[(generator.region == 'SC_VACA')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 28626 # 50% of new cc capex in the same region
      # generator.loc[(generator.region == 'SC_VACA')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 22403 # 50% of new ct capex in the same region
      if ((Settlement$Region[i] %in% c('PJM_NJCoast','PJM_NJLand','PJM_Delaware','PJM_PECO','PJM_SMAC','PJM_WestMAC'))&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')) {
        repowercost <- 27368
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] %in% c('PJM_NJCoast','PJM_NJLand','PJM_Delaware','PJM_PECO','PJM_SMAC','PJM_WestMAC'))&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        repowercost <- 20103
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'PJM_Dom')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
        repowercost <- 25298
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'PJM_Dom')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        repowercost <- 18237
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'PJM_COMD')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
        # generator.loc[(generator.region == 'PJM_COMD')&(generator.Resource == 'natural_gas_fired_combined_cycle'), 'Fixed_OM_cost_per_MWyr'] += 39265 # 50% of new cc capex in the same region
        repowercost <- 28692
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'PJM_COMD')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        # generator.loc[(generator.region == 'PJM_COMD')&(generator.Resource == 'natural_gas_fired_combustion_turbine'), 'Fixed_OM_cost_per_MWyr'] += 30307 # 50% of new ct capex in the same region
        repowercost <- 22146
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'PJM_WEST')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
        repowercost <- 22410
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'PJM_WEST')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        repowercost <- 17152
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'MIS_Central')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
        repowercost <- 24167
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'MIS_Central')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        repowercost <- 18817
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'MIS_East')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
        repowercost <- 24793
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'MIS_East')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        repowercost <- 19397
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'SC_TVA')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
        repowercost <- 21687
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'SC_TVA')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        repowercost <- 16975
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'SC_VACA')&(Settlement$Resource[i] == 'natural_gas_fired_combined_cycle')){
        repowercost <- 20917
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } else if ((Settlement$Region[i] == 'SC_VACA')&(Settlement$Resource[i] == 'natural_gas_fired_combustion_turbine')){
        repowercost <- 16370
        Settlement[i,] <- zcf_repowercostupdate(Settlement[i,], repowercost)
      } 
    }
  }
}
