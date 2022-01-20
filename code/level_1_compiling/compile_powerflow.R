# Compile power flow of GenX
# Created on Aug 6 2021
# Created by Qingyu Xu

if (exists(powerflow)){
  rm(powerflow)
}
print('begin compiling model cost data')

for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_flow_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],
                           "_",years[j],"_",cases[i],"/Results/flow.csv");
    if (file.exists(temp_flow_fn)){
      temp_flow = read_csv(temp_flow_fn, col_types = cols()) %>%
        rename(Time_index = Line) %>%
        filter(Time_index != 'Sum') %>%
        select(-Total) %>%
        mutate(Time_index = as.numeric(str_remove(Time_index,'t'))) %>%
        pivot_longer(cols=c(-Time_index),names_to = 'Line') %>% 
        mutate(case = cases[i], year = years[j])
      # long format is very space expensive
      if(!exists('powerflow')) {
        powerflow <- temp_flow;
      } else {
        powerflow <- rbind(powerflow, temp_flow);
      }
    }
  }
}

if (exists('powerflow')) {
  write_csv(powerflow, paste0(RunFdr,"/CompiledResults/powerflow.csv"));
  if (dim(interface_line_mapping)[1] >=1){
    interface_powerflow <- powerflow %>%
      left_join(interface_line_mapping, by = c('Line' = 'Interface_Line')) %>%
      group_by(Interface, case, year, Time_index) %>%
      summarise(value = sum(Interface_Line_Direction*value))
    write_csv(interface_powerflow, 
              paste0(RunFdr,"/CompiledResults/interfacepowerflow.csv"));
    print('finished calculating interface powerflow data')
  }
  rm(temp_flow_fn, temp_flow,powerflow)
  print('finished compiling powerflow data')
} else {
  print('there are no flow.csv')
}



  
