

#-------------------------------------------#
#Combining Generator data results -----
#-------------------------------------------#
if (exists('generators')){
  rm('generators','MaxoutCheck')
}
print('begin compiling generator data')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_generator_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                years[j],"_",cases[i],"/Inputs/Generators_data.csv");
    if (file.exists(temp_generator_fn)){
      temp_generators = read_csv(temp_generator_fn, col_types = cols()) %>%
        mutate(case = cases[i],
               year = years[j])
      # temp_generators$case = cases[i]
      # temp_generators$year = years[j]
    }
    if(!exists('generators')) {
      generators <- temp_generators;
    } else {
      generators <- rbind(generators, temp_generators);
    }
  }
}
if (exists('generators')){
  write_csv(generators, paste0(RunFdr,"/CompiledResults/generators.csv"));
  temp_capacity <- read_csv(paste0(RunFdr,"/CompiledResults/capacity.csv"), 
                            col_types = cols()) %>%
    mutate(Zone = as.numeric(Zone),
           Cluster = as.numeric(Cluster));
  # temp_capacity$Zone <-as.numeric(temp_capacity$Zone);
  # temp_capacity$Cluster <-as.numeric(temp_capacity$Cluster);
  MaxoutCheck <- merge(generators, temp_capacity,by.x = c("region","Resource","zone","cluster","case","year"), 
                       by.y = c("Region","Resource","Zone","Cluster","case","year"), all.x=T);
  MaxoutCheck <- aggregate(cbind(EndCap,Max_Cap_MW)~region+Resource+case+year,data = MaxoutCheck,sum)
  MaxoutCheck <- MaxoutCheck[(MaxoutCheck$Max_Cap_MW >0.1) & (abs(MaxoutCheck$Max_Cap_MW-MaxoutCheck$EndCap)<1),];
  write_csv(MaxoutCheck, paste0(RunFdr,"/CompiledResults/maxout.csv"));
  print('finished compiling generator data')
  print(Sys.time())
} else {
  print('there are no Generators_data.csv')
  print(Sys.time())
}
