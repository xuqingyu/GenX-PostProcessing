#------------------------------------#
#  Combining RPS constraint data ----
#------------------------------------#
if (exists('RPS_constr')){
  rm('RPS_constr')
}
print('begin compiling Energy share requirement data')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_RPS_constr_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Inputs/Energy_share_requirement.csv");
    if (file.exists(temp_RPS_constr_fn)){
      temp_RPS_constr = read_csv(temp_RPS_constr_fn, col_types = cols()) %>%
        pivot_longer(!c(`Region_description`, Network_zones),
                     names_to = 'RPS_Constraint_Name') %>%
        mutate(case = cases[i], year = years[j])
      if(!exists('RPS_constr')) {
        RPS_constr <- temp_RPS_constr;
      } else {
        RPS_constr <- rbind(RPS_constr, temp_RPS_constr);
      }
    }
  }
}
if(exists('RPS_constr')){
  write_csv(RPS_constr, paste0(RunFdr,"/CompiledResults/ESR_constraints.csv"));
  rm(temp_RPS_constr_fn, temp_RPS_constr)
  print('finish compiling Energy share requirement data')
  print(Sys.time())
} else {
  print('there are no Energy_share_requirement.csv files')
  print(Sys.time())
}
