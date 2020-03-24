#### Impact of COVID19 on hospital bed capacity BY UK REGIONS

library(ggplot2)
library(reshape2)
library(patchwork)
library(gridGraphics)
library(tidyverse)

#### FUNCTIONS
source("functions_hosp_covid.R")

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************

region_data <- read.csv("data/region_data.csv")[,-1]


####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************

#### NUMBERS
los_norm <- c(7.9,4) # ICHNT ICU NORMAL LOS 
los_cov <- readRDS("delay_dist_linelist.rds") #sample(seq(1,length(x),1), size = n, prob = x, replace=T) #c(10,8) # GUESS 

cols <- c("3" = "lightblue", "1" = "red", "0" = "darkgreen")

nruns = 10

ndays = 14 # 2 weeks

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************

rate_growth <- c(0.02, 0.1, 0.18)
rate_names <- c("low","med","high")
M_all_m <- c()
M_all_sd <- c()

# Run through the 7 regions of the UK. 
for(ii in 1:length(region_data[,1])){
  print(region_data[ii,1])
  nbeds_reg <- ceiling(region_data[ii,"nccbeds"])
  inc_rate_reg <- ceiling(region_data[ii,"inc_rate"])
  
  M_all_m <- matrix(0,14,4)
  M_all_m[,1] <- seq(1,14,1)
  M_all_sd <- matrix(0,14,4)
  M_all_sd[,1] <- seq(1,14,1)
  
  # Run through 3 scenarios - low, med, high
  for(jj in 1:3){
    cov_curve_reg <- exp(rate_growth[jj]*seq(1,ndays,1))# c(sigmoid(c(region_data[ii,"wuhan_covid"],0.3,30),seq(1,14,1)))
    
    M_wuh <- multiple_runs(nruns, nbeds = nbeds_reg, los_norm, los_cov, 
                           cov_curve = cov_curve_reg, ndays, inc_rate = inc_rate_reg)
    
    plot_multiple(M_wuh,paste0("region_",region_data[ii,1]))
    
    # Save
    tt <- M_wuh$total_beds_time
    M_all_m[tt$time,(jj+1)] <- tt$mbed
    M_all_sd[tt$time,(jj+1)] <- tt$sdbed
    
    # e.g. 
    norm_curve_reg <- rnorm(ndays,inc_rate_reg,1)
    output_eg <- bed_filling(nbeds_reg, los_norm, los_cov, cov_curve_reg,norm_curve_reg,ndays=14)
    plot_eg(output_eg, paste0("region_",region_data[ii,1],"_",rate_names[ii]), 
            norm_curve = norm_curve_reg, cov_curve = cov_curve_reg)
    
  }
  
  #####***** PLOT together
  M_all_m <- as.data.frame(M_all_m)
  colnames(M_all_m) <- c("time","high","med","low")
  M_all_mean <- melt(M_all_m, id.vars = "time") 
  M_all_mean$mean.need <- M_all_mean$value
  
  M_all_sd <- as.data.frame(M_all_sd)
  colnames(M_all_sd) <- c("time","high","med","low")
  M_all_sd <- melt(M_all_sd, id.vars = "time") 
  M_all_sd$sd.need <- M_all_sd$value
  
  M_all <- cbind(M_all_mean[,c("time","variable","mean.need")], M_all_sd$sd.need)
  colnames(M_all) <- c("time","variable","mean.need","sd.need")
  write.csv(M_all, paste0("outputs/region_all_mean_sd_",region_data[ii,1]))
  
  g <- ggplot(M_all,aes(x=time, y = mean.need, group = variable)) + 
    geom_ribbon(aes(group = variable, ymin = mean.need - sd.need, ymax = mean.need +sd.need, fill = variable), alpha = 0.4) + 
    geom_line(aes(y = mean.need)) + 
    scale_fill_discrete("Scenario") + 
    scale_y_continuous("Number of beds needed") + 
    scale_x_continuous("Days") + 
    geom_hline(yintercept = nbeds, col = "red",lty = "dashed") + theme_bw()
  
  ggsave(paste0("plots/all_bed_need_overtime_",region_data[ii,1],".pdf"))

  
}







#### Table of outputs

names <- region_data[,1]

ff_store <- c()
gg_store <- c()

for(i in 1:length(names)){
  ff <- read.csv(paste0("outputs/",names[i],"_extrabed.csv"))[,-1]
  gg <- read.csv(paste0("outputs/",names[i],"_totalmissed.csv"))[,-1]
  
  ff_store <- rbind(ff_store, c(ff))
  gg_store <- rbind(gg_store, c(gg))
}

ff_store <- as.data.frame(ff_store)
gg_store <- as.data.frame(gg_store)
colnames(ff_store) <- c("mean_extra","sd_extra")
colnames(gg_store) <- c("mean_total_norm","sd_total_norm", "mean_total_covid","sd_total_covid")


# Table
c.text <- function(xx){ xx <- as.numeric(as.vector(xx));paste0(round(xx[1],0)," (",round(xx[2],0),")")}
results_tab <- cbind(
  rbind(c.text(ff_store[1,1:2]),c.text(ff_store[2,1:2]),c.text(ff_store[3,1:2])),## beds needed
  round((100*as.numeric(as.vector(ff_store[,2]))/nbeds_uclh),0),# percentage current
  rbind(c.text(gg_store[1,1:2]),c.text(gg_store[2,1:2]),c.text(gg_store[3,1:2])),# normal missed
  rbind(c.text(gg_store[1,3:4]),c.text(gg_store[2,3:4]),c.text(gg_store[3,3:4])))# covid missed
results_tab <- cbind(names, results_tab)
write.csv(results_tab,"outputs/table_results_regions.csv")      