source("./code/Header.R")
# Supply-side ----
if(exists('CapRevenue')){
  rm('CapRevenue')
}
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_cap_res_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/CapRes.csv")
    temp_gen_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Generators_data.csv")
    temp_cap_price_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/ReserveMargin.csv")
    temp_cap_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/capacity.csv")
    temp_power_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/power.csv")
    temp_charge_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/charge.csv")
    if (file.exists(temp_cap_res_fn)){
      temp_cap_res <- read_csv(temp_cap_res_fn);
      NoCapmarket <- dim(temp_cap_res)[2]-2;
      NoZone <- dim(temp_cap_res)[1]
      
      temp_gen <- read_csv(temp_gen_fn)
      dr_row <-  which((temp_gen$THERM != 1) & (temp_gen$STOR == 0) & (temp_gen$DR >=1));
      thermal_row <- which(temp_gen$THERM == 1);
      temp_caprevenue_total <- temp_gen %>%
        select(c(region, Resource,cluster,R_ID,zone)) %>%
        mutate(item = 'Gen_CapRevenue',
               case = cases[i],
               year = years[j],
               value = 0);
      temp_caprevenue_total$item[dr_row] <- 'Con_DemandResponseCapRevenue';
      
      temp_endcap <- read_csv(temp_cap_fn) %>%
        filter(Region != 'n/a') %>%
        select(EndCap)
      temp_cap_price <- read.csv(temp_cap_price_fn,header = F)[-1,];
      NoGen <- nrow(temp_gen);
      if(exists('temp_caprevenue')){
        rm('tempname', 'temp_power','temp_charge','temp_capcontribution','temp_caprevenue')
      }
      for (k in (1:NoCapmarket)){
        tempname <- paste0('CapRes_',k)
        temp_derate <- temp_gen %>% select(tempname) %>% as.matrix()
        temp_power <- apply(t(read.csv(temp_power_fn,header = F)[,-c(1,NoGen+1)])[,-c(1:3)],2,as.numeric)
        temp_charge <- apply(t(read.csv(temp_charge_fn,header = F)[,-c(1,NoGen+1)])[,-c(1:3)],2,as.numeric)
        temp_capcontribution <- temp_power - temp_charge;
        temp_capcontribution[dr_row,] <- (-1)*temp_capcontribution[dr_row,];
        #temp_capcontribution[dr_row,] <- temp_charge[dr_row,];
        temp_capcontribution[thermal_row,] <- as.matrix(temp_endcap[thermal_row,]);
        derate <- function(x){x = x*temp_derate}
        temp_capcontribution <- apply(temp_capcontribution,2,derate);
        temp_caprevenue <- temp_capcontribution %*% as.numeric(as.matrix(temp_cap_price[k,]))
        temp_caprevenue_total$value<- temp_caprevenue_total$value + temp_caprevenue
      }
      if(!exists('CapRevenue')){
        CapRevenue <- temp_caprevenue_total
      } else {
        CapRevenue <- rbind(CapRevenue,temp_caprevenue_total)
      }
    }
  }
}
CapRevenue_zone <- CapRevenue %>%
  group_by(zone,case,year,item) %>%
  summarise(value = sum(value))

# Demand-side ----
if(exists('CapCost')){
  rm('CapCost')
}
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_cap_res_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/CapRes.csv")
    temp_cap_price_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/ReserveMargin.csv")
    
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
    nse_nofirstseg <- as_tibble(nse) %>%
      filter(Segment != 1)%>%
      pivot_longer(c(3:(NoHour+2)),names_to = 'hour',values_to = 'nse_mw') %>%
      group_by(zone,hour) %>%
      summarize(nse_mw = sum(nse_mw));
    nse_nofirstseg$hour <- as.numeric(nse_nofirstseg$hour);

    if (file.exists(temp_cap_res_fn)){
      if (exists('temp_cap_res')){
        rm('temp_cap_res','temp_capcost','temp_nse_saving')
      }
      temp_cap_res <- read_csv(temp_cap_res_fn);
      NoCapmarket <- dim(temp_cap_res)[2]-2;
      temp_capcost <- temp_cap_res %>%
        mutate(zone = c(1:NoZone)) %>%
        mutate(item = 'Con_CapCost',
               case = cases[i],
               year = years[j],
               value = 0) %>%
        select(zone,case,year,item,value);
      temp_nse_saving <- temp_cap_res %>%
        mutate(zone = c(1:NoZone)) %>%
        mutate(item = 'Con_LoadManagement_CapRevenue',
               case = cases[i],
               year = years[j],
               value = 0)%>%
        select(zone,case,year,item,value);
      temp_cap_price <- read.csv(temp_cap_price_fn,header = F)[-1,] %>%
        t() %>%
        as_tibble() %>%
        mutate(hour = c(1:NoHour));
      colnames(temp_cap_price)[c(1:2)] <- colnames(temp_cap_res)[-c(1,2)]

      for (k in (1:NoCapmarket)){
        if(exists('temp_capcost_market')){
          rm('tempname','temp_cap_price_market', 
             'temp_capacitymargin','temp_capacitytarget',
             'temp_capcost_per_market','temp_nse_nofirstseg','temp_cap_nse_saving_permarket')
        }
        tempname <- paste0('CapRes_',k)
        temp_capacitymargin <- temp_cap_res %>% select(tempname) %>% mutate(zone = c(1:NoZone));
        temp_cap_price_market <- temp_cap_price %>% select(tempname,hour);
        colnames(temp_cap_price_market)[1] <- 'CapPrice';
        temp_cap_price_market$CapPrice <- as.numeric(temp_cap_price_market$CapPrice);
        colnames(temp_capacitymargin)[1] <- 'RM'
        
        temp_capacitytarget <- left_join(load_data, temp_capacitymargin, by = 'zone') %>%
          mutate(target = (1+RM)*load_mw);
        temp_capacitytarget$target[which(temp_capacitytarget$RM<0.001)]<-0
        temp_capcost_per_market <- left_join(temp_capacitytarget,temp_cap_price_market, by = 'hour') %>%
          group_by(zone) %>%
          summarize(value = sum(target*CapPrice));
        temp_capcost$value = temp_capcost$value + as.numeric(temp_capcost_per_market$value)
        
        temp_nse_nofirstseg <- left_join(nse_nofirstseg, temp_capacitymargin, by = 'zone');
        temp_nse_nofirstseg$nse_mw[which(temp_nse_nofirstseg$RM<0.001)]<-0;
        temp_cap_nse_saving_permarket <- left_join(temp_nse_nofirstseg, temp_cap_price_market, by = 'hour') %>%
          group_by(zone) %>%
          summarize(value = sum(nse_mw*CapPrice))
        temp_nse_saving$value = temp_nse_saving$value + as.numeric(temp_cap_nse_saving_permarket$value)
      }
      if(!exists('CapCost')){
        CapCost <- rbind(temp_capcost, temp_nse_saving);
      } else {
        CapCost <- rbind(CapCost,temp_capcost, temp_nse_saving)
      }
    }
  }
}

# Transmission side ----
if(exists('TransmissionCapRev')){
  rm('TransmissionCapRev')
}
for ( i in 1:length(cases)){
  for (j in 1:length(years)){
    temp_cap_res_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/CapRes.csv")
    temp_network_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Inputs/Network.csv")
    temp_cap_price_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/ReserveMargin.csv")
    temp_flow_fn <- paste0(RunFdr,"/",years[j],"/",case_ids[i],"_",years[j],"_",cases[i],"/Results/flow.csv")
    if (file.exists(temp_cap_res_fn)){
      temp_cap_res <- read_csv(temp_cap_res_fn);
      NoCapmarket <- dim(temp_cap_res)[2]-2;
      NoZone <- dim(temp_cap_res)[1]
      Networkcol <- c('Network_lines',paste0('DerateCapRes_',c(1:NoCapmarket)),paste0('CapRes_Excl_',c(1:NoCapmarket)));
      temp_network <- read_csv(temp_network_fn) %>% select(all_of(Networkcol)) %>% na.omit()
      temp_trans_caprevenue_total <- temp_network %>%
        select(c(Network_lines)) %>%
        mutate(item = 'Trans_CapRevenue',
               case = cases[i],
               year = years[j],
               value = 0);
      temp_cap_price <- read.csv(temp_cap_price_fn,header = F)[-1,];
      NoLine <- nrow(temp_network)
      if(exists('temp_trans_caprevenue')){
        rm('temp_derate','temp_direction','temp_flow','temp_trans_capcontribution','temp_trans_caprevenue')
      }
      for (k in (1:NoCapmarket)){
        temp_derate <- temp_network %>% select(paste0('DerateCapRes_',k)) %>% as.matrix()
        temp_direction <- temp_network %>% select(paste0('CapRes_Excl_',k)) %>% as.matrix()
        temp_flow <- apply(t(read.csv(temp_flow_fn,header = F)[,-c(1,NoLine+1)])[,-c(1:2)],2,as.numeric)
        temp_trans_capcontribution <- temp_flow;
        derate <- function(x){x = x*temp_derate*temp_direction*(-1)}
        temp_trans_capcontribution <- apply(temp_trans_capcontribution,2,derate);
        temp_trans_caprevenue <- temp_trans_capcontribution %*% as.numeric(as.matrix(temp_cap_price[k,]))
        temp_trans_caprevenue_total$value<- temp_trans_caprevenue_total$value + temp_trans_caprevenue
      }
      if(!exists('TransmissionCapRev')){
        TransmissionCapRev <- temp_trans_caprevenue_total
      } else {
        TransmissionCapRev <- rbind(TransmissionCapRev,temp_trans_caprevenue_total)
      }
    }
  }
}

# write output ---- 

write_csv(CapRevenue, paste0(RunFdr,"/CompiledResults/Gen_Capacity_Settlement.csv"))
write_csv(rbind(CapRevenue_zone,CapCost),paste0(RunFdr,"/CompiledResults/Zonal_Capacity_Settlement.csv"))
write_csv(TransmissionCapRev,paste0(RunFdr,"/CompiledResults/Transmisssion_Capacity_Settlement.csv"))
sum(CapRevenue$value)+sum(TransmissionCapRev$value) +sum(CapCost$value[which(CapCost$item == 'Con_LoadManagement_CapRevenue')]) - sum(CapCost$value[which(CapCost$item == 'Con_CapCost')]) 
