#--------------------------------------#
#Combining CO2 Massbased data -----
#--------------------------------------#
if (exists('LSECO2Revenue_mass')){
  rm('LSECO2Revenue_mass')
}
print('begin compile CO2 mass cap lse revenue')
print(Sys.time())
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSECO2Revenue_mass_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],
                                         "_",years[j],"_",cases[i],
                                         "/Results/CO2Revenue_mass.csv");
    if (file.exists(temp_LSECO2Revenue_mass_fn)){
      temp_LSECO2Revenue_mass = read_csv(temp_LSECO2Revenue_mass_fn, 
                                         col_types = cols(), col_names = T)
      end = dim(temp_LSECO2Revenue_mass)[2]
      # n_constraint = end - 2
      # colnames(temp_LSECO2Revenue_mass) <- c('Zone', 'AnnualSum', paste('CO2_MassCap_Revenue_',c(1:n_constraint), sep = ''))
      temp_LSECO2Revenue_mass = pivot_longer(temp_LSECO2Revenue_mass[-2], 
                                             c(2:(end-1)), # column 2 is the sum
                                             names_to = "item") %>%
        mutate(case = cases[i], year = years[j])
      if (!exists('LSECO2Revenue_mass')) {
        LSECO2Revenue_mass <- temp_LSECO2Revenue_mass;
      } else {
        LSECO2Revenue_mass <- rbind(LSECO2Revenue_mass, temp_LSECO2Revenue_mass);
      }
    }
  }
}
if (exists('LSECO2Revenue_mass')) {
  write_csv(LSECO2Revenue_mass, paste0(RunFdr,"/CompiledResults/LSECO2Revenue_mass.csv"));
  print('finished compile CO2 mass cap lse revenue')
  print(Sys.time())
} else {
  print('there are no CO2Revenue_mass.csv')
  print(Sys.time())
}
