#--------------------------------------#
# Combining energy price results----
#--------------------------------------#


if (exists('prices')){
  rm('prices','prices_sum')
}
print('begin compiling energy prices')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_prices_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/prices_w.csv");
    temp_timeweight_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/time_weights.csv");     
    if (file.exists(temp_prices_fn)){
      temp_prices = as.data.frame(read.csv(temp_prices_fn));
      colnames(temp_prices) <- c("hour",zone_mapping$region);
      temp_prices$case = cases[i]
      temp_prices$year = years[j]
      time_weight = read.csv(temp_timeweight_fn)
      temp_prices$weight = time_weight$Weight
      temp_prices <- temp_prices %>%
        pivot_longer(zone_mapping$region);
    }
    if(!exists('prices'))
    {
      prices <- temp_prices;
    }
    else
    {
      prices <- rbind(prices, temp_prices);
    }
  }
}

if (exists('prices')){
  prices_sum <- prices %>%
    group_by(case,year,name) %>%
    summarize(AnnualPrice = sum(weight*value)/8760);
  write_csv(prices, paste0(RunFdr,"/CompiledResults/prices.csv"))
  write_csv(prices_sum,paste0(RunFdr,"/CompiledResults/price_summary.csv"))
  print('finished compiling energy prices')
} else {
  print('there are no price_w.csv files')
}
