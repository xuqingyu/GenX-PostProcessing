
if(exists('cfe_table')) {rm(cfe_table, cfe_table_withname)}
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    # CFE Score
    Policy = as_tibble_col(c(1:n_tfs), column_name = 'Policy')
    temp_cfe_score_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                          years[j],"_",cases[i],"/Results/tfs_cfe_score.csv");
    temp_cfe_score <- read_csv(temp_cfe_score_fn, col_types = cols()) %>%
      select(cfe_score) %>%
      rename(`Post-Grid CFE Score Local_n_Import` = cfe_score)
    temp_cfe_load_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Results/tfs_part_load_emissions.csv");
    temp_cfe_load <- read_csv(temp_cfe_load_fn, col_types = cols()) %>%
      select(participatedload, storageloss) %>%
      rename(`Load` = participatedload,
             `Storage loss` = storageloss)
    #Short fall and excess    
    temp_sf_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Results/tfs_sf.csv");
    temp_sf_price_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                               years[j],"_",cases[i],"/Results/tfs_shortfalllimitprice.csv");
    temp_ex_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                         years[j],"_",cases[i],"/Results/tfs_ex.csv");
    temp_ex_price_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                         years[j],"_",cases[i],"/Results/tfs_exceedlimitprice.csv");
    temp_timeweight_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                 years[j],"_",cases[i],"/Results/time_weights.csv");
    temp_sf_dirtiness_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                   years[j],"_",cases[i],"/Inputs/RPSH_SFDT.csv");
    if (file.exists(temp_sf_fn)) {
      temp_sf_ori <- data.matrix(read_csv(temp_sf_fn, col_types = cols())[-1,-1])
      temp_ex_ori <- data.matrix(read_csv(temp_ex_fn, col_types = cols())[-1,-1])

      temp_timeweight <- data.matrix(read_csv(temp_timeweight_fn, col_types = cols())[,-1])
      temp_sf_dirtiness <- data.matrix(read_csv(temp_sf_dirtiness_fn, col_types = cols())[,-1])
      temp_sf = temp_sf_ori - temp_ex_ori
      temp_sf = apply(temp_sf, 2, negtozero)
      temp_ex = temp_ex_ori - temp_sf_ori
      temp_ex = apply(temp_ex, 2, negtozero)
      temp_sf_total <- as_tibble_col(t(temp_sf*temp_sf_dirtiness) %*% temp_timeweight, column_name = 'Shortfall')
      temp_ex_total <- as_tibble_col(t(temp_ex) %*% temp_timeweight, column_name = 'Excess')
      temp_sf_price <- read_csv(temp_sf_price_fn, col_types = cols())[,2] %>%
        rename(`Shortfall price` = Price)
      temp_ex_price <- read_csv(temp_ex_price_fn, col_types = cols())[,2] %>%
        rename(`Excess price` = Price)
      rm(temp_sf_ori,temp_ex_ori,temp_timeweight,temp_sf_dirtiness,temp_sf,temp_ex)
    } else {
      temp_sf_total <- as_tibble_col(rep(0,n_tfs), column_name = 'Shortfall')
      temp_ex_total <- as_tibble_col(rep(0,n_tfs), column_name = 'Excess')
      temp_sf_price <- as_tibble_col(rep(0,n_tfs), column_name = 'Shortfall price')
      temp_ex_price <- as_tibble_col(rep(0,n_tfs), column_name = 'Excess price')
    }
    temp_cfe_table = cbind(Policy, 
                           temp_sf_total,
                           temp_ex_total,
                           temp_sf_price,
                           temp_ex_price,
                           temp_cfe_load,
                           temp_cfe_score) %>%
      mutate(case = cases[i], year = years[j])
    rm(temp_sf_total, temp_ex_total,temp_sf_price, temp_ex_price,temp_cfe_load,temp_cfe_score)

    rm(temp_cfe_score_fn,temp_cfe_load_fn,temp_sf_fn,temp_sf_price_fn,temp_ex_fn,
       temp_ex_price_fn,temp_timeweight_fn,temp_sf_dirtiness_fn)
    if (!exists('cfe_table')) {
      cfe_table <- temp_cfe_table
      rm(temp_cfe_table)
    } else {
      cfe_table <- rbind(cfe_table, temp_cfe_table)
      rm(temp_cfe_table)
    }
  }
}
cfe_table_withname <- cfe_table %>%
  left_join(cases_newnames, by = c('case' = 'case_description'))
write_csv(cfe_table_withname, paste0(RunFdr,"/CompiledResults/tfs_cfe_table.csv"))
