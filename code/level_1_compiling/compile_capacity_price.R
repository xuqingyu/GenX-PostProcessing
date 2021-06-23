
#----------------------------------------------------#
#Combining Capacity Reserve Market results --------
#----------------------------------------------------#
if (exists('Res_Mar')){
  rm('Res_Mar')
}
print('begin compiling capacity market price')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_Res_Mar_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/ReserveMargin_w.csv");
    temp_timeweight_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/time_weights.csv");     
    if (file.exists(temp_Res_Mar_fn)){
      temp_Res_Mar = read_csv(temp_Res_Mar_fn) %>%
        rename(hour = Constraint)
      time_weight = read.csv(temp_timeweight_fn);
      temp_Res_Mar$weight = time_weight$Weight;
      temp_Res_Mar$case = cases[i]
      temp_Res_Mar$year = years[j];
      temp_Res_Mar <- temp_Res_Mar %>%
        pivot_longer(cols=!c('hour','weight','case','year'));
    }
    if(!exists('Res_Mar')) {
      Res_Mar <- temp_Res_Mar;
    } else {
      Res_Mar <- rbind(Res_Mar, temp_Res_Mar);
    }
  }
}
if (exists('Res_Mar')) {
  write_csv(Res_Mar, paste0(RunFdr,"/CompiledResults/Res_Mar.csv"));
  print('finished compiling capacity market price')
} else {
  print('there are no ReserveMargin_w.csv files')
}
