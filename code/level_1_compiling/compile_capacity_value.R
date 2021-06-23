# Combining Capacity Reserve Value results----
if (exists('CapValue')){
  rm('CapValue','CapValue_Hour')
}
print('begin compiling capacity value')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_capvalue_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/CapacityValue.csv");
    temp_power_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/power.csv");
    temp_timeweight_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/time_weights.csv");    
    if (file.exists(temp_capvalue_fn)){
      temp_CapValue = read_csv(temp_capvalue_fn);
      time_weight = read_csv(temp_timeweight_fn)
      n_hours <- dim(time_weight)[1]
      temp_CapValue <- temp_CapValue[,c(1,2,(n_hours+3),3:(n_hours+2))]; # Reorder the results
      temp_CapValue_hour <- temp_CapValue;
      temp_CapValue_hour[,c(4:(n_hours+3))] <- ceiling(temp_CapValue_hour[,c(4:(n_hours+3))]);
      temp_CapValue_hour$sum <- as.matrix(temp_CapValue_hour[,c(4:(n_hours+3))]) %*% time_weight$Weight
      temp_CapValue$value <- as.matrix(temp_CapValue[,c(4:(n_hours+3))]) %*% time_weight$Weight
      temp_CapValue$value <- temp_CapValue$value/temp_CapValue_hour$sum;
      temp_CapValue<-select(temp_CapValue, Resource, Zone, Reserve, value)
      temp_CapValue_hour <-select(temp_CapValue_hour, Resource, Zone, Reserve, sum)
      temp_CapValue_hour$case = cases[i];
      temp_CapValue_hour$year = years[j];
      temp_CapValue$case = cases[i];
      temp_CapValue$year = years[j];
      
      t_power = t(read.csv(temp_power_fn,header = F)[1:3,]);
      powercolnames <- t_power[1,];
      t_power <- t_power[-c(1,length(t_power[,1])),];
      colnames(t_power) <- powercolnames;
      t_power <- as.data.frame(t_power);
      n_reserve <- length(unique(temp_CapValue$Reserve))
      annual_sum <- rep(t_power$Sum,n_reserve)
      temp_CapValue <-cbind(temp_CapValue,annual_sum) 
    }
    if(!exists('CapValue')) {
      CapValue <- temp_CapValue;
      CapValue_Hour <- temp_CapValue_hour;
    } else {
      CapValue <- rbind(CapValue, temp_CapValue);
      CapValue_Hour <- rbind(CapValue_Hour,temp_CapValue_hour )
    }
  }
}
if (exists('CapValue')){
  write_csv(CapValue, paste0(RunFdr,"/CompiledResults/CapValue.csv"));
  write_csv(CapValue_Hour, paste0(RunFdr,"/CompiledResults/CapValue_hour.csv"));
  rm(CapValue, CapValue_Hour,temp_CapValue,temp_CapValue_hour,n_reserve,t_power,powercolnames,n_hours,temp_timeweight_fn,time_weight,temp_capvalue_fn,temp_power_fn)
  print('finished compiling capacity value')
} else {
  print('there are no capacity value files')
}
