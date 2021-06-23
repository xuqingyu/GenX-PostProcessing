#--------------------------------------#
#Combining CO2 Massbased data -----
#--------------------------------------#
if (exists('LSECO2Revenue_mass')){
  rm('LSECO2Revenue_mass')
}
print('begin compile CO2 mass cap lse revenue')
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_LSECO2Revenue_mass_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/CO2Revenue_mass.csv");
    if (file.exists(temp_LSECO2Revenue_mass_fn)){
      temp_LSECO2Revenue_mass = read.csv(temp_LSECO2Revenue_mass_fn)
      end = dim(temp_LSECO2Revenue_mass)[2]
      temp_LSECO2Revenue_mass = pivot_longer(temp_LSECO2Revenue_mass[-2], c(2:(end-1)),names_to = "item") # column 2 is the sum
      temp_LSECO2Revenue_mass$case = cases[i]
      temp_LSECO2Revenue_mass$year = years[j]
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
} else {
  print('there are no CO2Revenue_mass.csv')
}