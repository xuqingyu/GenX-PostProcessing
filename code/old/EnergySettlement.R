source("./code/Header.R")
if (exists('ExportRevenue')){
  rm('ExportRevenue', 'CongestionRent')
}
if (exists('NetEnergyRevenue')){
  rm('NetEnergyRevenue', 'ConsumerEnergyPayment')
}

# Calculate Import and Export Revenue ----
for ( i in 1:length(cases))
{
  for (j in 1:length(years))
  {
    temp_load <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Load_data.csv")
    if (file.exists(temp_load)){
      load_data <- read_csv(temp_load) %>%
        select(-c(1:8));
      NoZone <- dim(load_data)[2]-1;
      NoHour <- dim(load_data)[1];
      temp <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/prices.csv");
      if (NoZone > 1 & file.exists(temp))
      {
        network <- read_csv(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Network.csv")) %>%
          select(Network_lines,paste("z",1:NoZone,sep=""));
        colnames(network) <- c('line',1:NoZone);
        NoLine <- dim(network)[1];
        network <- network %>% 
          pivot_longer(c(2:(NoZone+1)), names_to = 'zone',values_to = 'from_one');
        network$line <- as.numeric(network$line)
        network$zone <- as.numeric(network$zone)
        
        flow <- read_csv(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/flow.csv")) %>%
          filter(Line != 'Sum') %>%
          select(-last_col()) %>%
          rename(hour = Line) %>%
          mutate(hour = c(1:NoHour)) %>%
          pivot_longer(c(2:(NoLine+1)), names_to = 'line', values_to = 'powerflow_mw');
        flow$line <- as.numeric(flow$line);
        export <- left_join(flow, network, by = 'line' ) %>%
          group_by(hour,zone) %>%
          summarize(export_mw = sum(powerflow_mw*from_one))
        temp_price_weighted <- read_csv(temp) %>%
          rename(hour = Zone) %>%
          mutate(hour = 1:NoHour) %>%
          pivot_longer(c(2:(NoZone+1)),names_to = 'zone', values_to = 'price_weighted');
        temp_price_weighted$zone <- as.numeric(temp_price_weighted$zone);
        temp_congestionrent <- left_join(temp_price_weighted,network,by='zone') %>%
          group_by(line,hour) %>%
          summarize(price_difference = sum(price_weighted*from_one)) %>%
          left_join(flow,by = c('line','hour')) %>%
          ungroup() %>% 
          group_by(line) %>%
          summarize(congestionrent = -1*sum(price_difference*powerflow_mw)) %>%
          mutate(case = cases[i],year = years[j]);
        temp_ExportRevenue <- left_join(export, temp_price_weighted, by = c('zone','hour')) %>%
          group_by(zone) %>%
          summarize(value = sum(price_weighted*export_mw)) %>%
          mutate(case = cases[i],year = years[j], item = 'ExportRevenue') %>%
          select(zone,case,year,item,value)
        if(!exists('ExportRevenue'))
        {
          ExportRevenue <- temp_ExportRevenue;
          CongestionRent <- temp_congestionrent
        }
        else
        {
          ExportRevenue <- rbind(ExportRevenue,temp_ExportRevenue);
          CongestionRent <-  rbind(CongestionRent,temp_congestionrent);
        }
      }
    }
  }
}



# Read in Energy Revenue - Supply Side ----
for ( i in 1:length(cases))
{
  for (j in 1:length(years))
  {
    # '[...]/[Running_folder]/[year]/[case_id]_[year]_[case_description]/Results/'
    temp <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/NetRevenue.csv");
    dr_id <- read_csv(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Generators_data.csv")) %>%
      select(region, Resource, cluster, R_ID, zone, DR);
    dr_row = which(dr_id$DR >0);
    if (file.exists(temp))
    {
      
      temp_NetEnergyRevenue <- read_csv(temp) %>%
        select( zone , EnergyRevenue, Charge_cost) %>%
        mutate(Gen_NetEnergyRevenue = EnergyRevenue - Charge_cost) %>%
        select(-c(EnergyRevenue, Charge_cost)) %>%
        pivot_longer(c('Gen_NetEnergyRevenue'), names_to = 'item', values_to = 'value') %>%
        mutate(case = cases[i],year = years[j]);
      
      temp_NetEnergyRevenue$value[dr_row] <- (-1)*temp_NetEnergyRevenue$value[dr_row];
      temp_NetEnergyRevenue$item[dr_row] <- 'Con_DemandResponseSaving';
      temp_NetEnergyRevenue <- temp_NetEnergyRevenue %>%
        group_by(zone, case, year,item) %>%
        summarize(value = sum(value)) %>%
        select(zone,case,year,item,value)
      temp_NetEnergyRevenue$zone <- as.numeric(temp_NetEnergyRevenue$zone);
      if (!exists('NetEnergyRevenue'))
      {
        NetEnergyRevenue <- temp_NetEnergyRevenue
      }
      else
      {
        NetEnergyRevenue <- rbind(NetEnergyRevenue, temp_NetEnergyRevenue)
      }
    }
  }
}

# Read in Energy Cost - Demand Side ----
for ( i in 1:length(cases))
{
  for (j in 1:length(years))
  {
    # '[...]/[Running_folder]/[year]/[case_id]_[year]_[case_description]/Results/'
    temp <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/prices.csv");
    load_data <- read_csv(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Load_data.csv")) %>%
      select(-c(1:8));
    NoZone <- dim(load_data)[2]-1;
    NoHour <- dim(load_data)[1];
    colnames(load_data) <- c('hour',1:NoZone);
    load_data <- load_data %>%
      pivot_longer(c(2:(NoZone+1)), names_to = 'zone',values_to = 'load_mw');
    load_data$zone <- as.numeric(load_data$zone);
    
    nse <- read_csv(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/nse.csv"),col_names = FALSE);
    nse <- nse %>% 
      filter(X1 != 'Sum') %>%
      select(-last_col()) %>%
      column_to_rownames(var = 'X1') %>%
      t();
    colnames(nse) <- c('Segment','zone',1:NoHour)
    nse <- as_tibble(nse) %>%
      pivot_longer(c(3:(NoHour+2)),names_to = 'hour',values_to = 'nse_mw') %>%
      group_by(zone,hour) %>%
      summarize(nse_mw = sum(nse_mw));
    nse$hour <- as.numeric(nse$hour);
    load_data <- left_join(load_data,nse, by = c('hour','zone'))
    
    if (NoZone >1) 
    {
      network <- read_csv(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Network.csv")) %>%
        select(Network_lines,paste("z",1:NoZone,sep=""));
      colnames(network) <- c('line',1:NoZone);
      NoLine <- dim(network)[1];
      network <- network %>% 
        pivot_longer(c(2:(NoZone+1)), names_to = 'zone',values_to = 'loss_coef');
      network$loss_coef[network$loss_coef != 0] <- 0.5;
      network$zone <- as.numeric(network$zone);
      loss <- read_csv(paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/tlosses.csv")) %>%
        filter(Line != 'Sum') %>%
        mutate(Line = 1:NoHour) %>%
        rename(hour = Line) %>%
        select(-Total) %>%
        pivot_longer(c(2:(NoLine+1)),names_to = 'line',values_to = 'loss_mw');
      loss$line <- as.numeric(loss$line);
      loss$loss_mw <- as.numeric(loss$loss_mw)
      loss <- left_join(loss,network, by = c('line')) %>%
        group_by(zone,hour) %>%
        summarize(loss_mw = sum(loss_mw*loss_coef))
      load_data <- left_join(load_data,loss, by = c('zone','hour'))
    }
    else
    {
      load_data <- load_data %>% 
        mutate(loss_mw = 0)
    }
    
    if (file.exists(temp))
    {
      temp_ConsumerEnergyPayment <- read_csv(temp) %>%
        rename(hour = Zone) %>%
        mutate(hour = 1:NoHour) %>%
        pivot_longer(c(2:(NoZone+1)),names_to = 'zone', values_to = 'price_weighted');
      temp_ConsumerEnergyPayment$zone <- as.numeric(temp_ConsumerEnergyPayment$zone);
      temp_ConsumerEnergyPayment <- temp_ConsumerEnergyPayment %>%
        left_join(load_data, by = c('zone','hour')) %>%
        group_by(zone) %>%
        summarize(Con_EnergyPayment = sum(price_weighted*(load_mw - nse_mw)),
                  Trans_LossCost = sum(price_weighted*loss_mw)) %>%
        pivot_longer(c('Con_EnergyPayment','Trans_LossCost'),names_to = 'item',values_to = 'value') %>%
        mutate(case = cases[i],year = years[j]) %>%
        select(zone,case,year,item,value);
      if (!exists('ConsumerEnergyPayment'))
      {
        ConsumerEnergyPayment <- temp_ConsumerEnergyPayment
      }
      else
      {
        ConsumerEnergyPayment <- rbind(ConsumerEnergyPayment, temp_ConsumerEnergyPayment)
      }
    }
    else
    {
      print(paste0('Energy Price data does not exist for case for', cases[i],'of year', years[j],'.'))
    }
  }
}





# Write output ----

Energy <- rbind(NetEnergyRevenue, ExportRevenue, ConsumerEnergyPayment);

Energy_wide <- Energy %>%
  pivot_wider(names_from = 'item',values_from = 'value');

write_csv(Energy, paste0(RunFdr,"/CompiledResults/EnergySettlement.csv"))
CongestionRent_summary <- CongestionRent %>%
  group_by(case,year) %>%
  summarize(congestionrent = sum(congestionrent))
write_csv(CongestionRent, paste0(RunFdr,"/CompiledResults/CongestionRent.csv"))

