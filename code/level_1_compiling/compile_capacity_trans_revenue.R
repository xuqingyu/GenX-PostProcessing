#-----------------------------------------#
#Combining LSE Capacity Payment data -----
#-----------------------------------------#
if (exists('TransCapacityRevenue')){
  rm('TransCapacityRevenue')
}
print('begin compiling transmission capacity revenue')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_TransCapacityRevenue_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],
                                           "_",years[j],"_",cases[i],
                                           "/Results/ReserveMarginTransRevenue.csv");
    if (file.exists(temp_TransCapacityRevenue_fn)){
      temp_TransCapacityRevenue = read_csv(temp_TransCapacityRevenue_fn, 
                                           col_types = cols())
      end = dim(temp_TransCapacityRevenue)[2]
      temp_TransCapacityRevenue = pivot_longer(temp_TransCapacityRevenue[-end], 
                                               c(2:(end-1)),
                                               names_to = "item") %>%
        mutate(case = cases[i], year = years[j])
      # temp_TransCapacityRevenue$case = cases[i]
      # temp_TransCapacityRevenue$year = years[j]
      if(!exists('TransCapacityRevenue')) {
        TransCapacityRevenue <- temp_TransCapacityRevenue;
      } else {
        TransCapacityRevenue <- rbind(TransCapacityRevenue, temp_TransCapacityRevenue);
      }
    }
  }
}
if (exists('TransCapacityRevenue')){
  write_csv(TransCapacityRevenue, paste0(RunFdr,"/CompiledResults/TransCapacityRevenue.csv"));
  print('finished compiling transmission capacity revenue')
  print(Sys.time())
} else {
  print('there are no ReserveMarginTransRevenue.csv')
  print(Sys.time())
}