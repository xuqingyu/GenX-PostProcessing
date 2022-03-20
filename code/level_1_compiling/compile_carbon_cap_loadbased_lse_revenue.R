#--------------------------------------#
#Combining CO2 LoadRate data -----
#--------------------------------------#
if (exists('LSECO2Revenue_loadrate')){
  rm('LSECO2Revenue_loadrate')
}
print('begin compiling CO2 loadrate based cap revenue')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSECO2Revenue_loadrate_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                                             years[j],"_",cases[i],
                                             "/Results/CO2Revenue_loadrate.csv");
    if (file.exists(temp_LSECO2Revenue_loadrate_fn)){
      temp_LSECO2Revenue_loadrate = read_csv(temp_LSECO2Revenue_loadrate_fn, 
                                             col_types = cols())
      end = dim(temp_LSECO2Revenue_loadrate)[2]
      temp_LSECO2Revenue_loadrate = pivot_longer(temp_LSECO2Revenue_loadrate[-2], 
                                                 c(2:(end-1)), # column 2 is the sum
                                                names_to = "item") %>%
        mutate(case = cases[i], year = years[j] )
      if(!exists('LSECO2Revenue_loadrate')) {
        LSECO2Revenue_loadrate <- temp_LSECO2Revenue_loadrate;
      } else {
        LSECO2Revenue_loadrate <- rbind(LSECO2Revenue_loadrate, temp_LSECO2Revenue_loadrate);
      }
    }
  }
}
if (exists('LSECO2Revenue_loadrate')) {
  write_csv(LSECO2Revenue_loadrate, paste0(RunFdr,"/CompiledResults/LSECO2Revenue_loadrate.csv"));
  print('finished compiling CO2 loadrate based cap revenue')
  print(Sys.time())
} else {
  print('there is are no CO2Revenue_loadrate.csv')
  print(Sys.time())
}