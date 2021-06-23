#--------------------------------------#
#Combining Transmission Congestion Revenue data -----
#--------------------------------------#
if (exists('TransCongestionRevenue')){
  rm('TransCongestionRevenue')
}
print('begin compiling congestio revenue')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_TransCongestionRevenue_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/CongestionRevenue.csv");
    if (file.exists(temp_TransCongestionRevenue_fn)){
      temp_TransCongestionRevenue = read_csv(temp_TransCongestionRevenue_fn) %>%
        mutate(case = cases[i], year = years[j])
      if(!exists('TransCongestionRevenue')) {
        TransCongestionRevenue <- temp_TransCongestionRevenue;
      } else {
        TransCongestionRevenue <- rbind(TransCongestionRevenue, temp_TransCongestionRevenue);
      }
    }
  }
}
if (exists('TransCongestionRevenue')) {
  write_csv(TransCongestionRevenue, paste0(RunFdr,"/CompiledResults/TransCongestionRevenue.csv"));
  print('finished compiling congestion revenue')
} else {
  print('There are no CongestionRevenue.csv')
}