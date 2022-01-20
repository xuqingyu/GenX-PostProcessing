#---------------------------------------------#
# Combining Transmission Expansion results----
#---------------------------------------------#
if (exists('trans')){
  rm('trans')
}
print('begin compiling transmission data')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_trans_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",
                            years[j],"_",cases[i],
                            "/Results/network_expansion.csv");
    if (file.exists(temp_trans_fn)){
      temp_trans = read_csv(temp_trans_fn,col_types = cols())
      temp_trans$case = cases[i]
      temp_trans$year = years[j]
    }
    if(!exists('trans')) {
      trans <- temp_trans;
    } else {
      trans <- rbind(trans, temp_trans);
    }
  }
}
if (exists('trans')){
  write_csv(trans, paste0(RunFdr,"/CompiledResults/trans.csv"));
  rm(temp_trans_fn, temp_trans,trans )
  print('finished compiling transmission data')
  print(Sys.time())
} else {
  print('there are no network_expansion.csv files')
  print(Sys.time())
}

