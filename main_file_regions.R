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

ndays = 90 # run for 3 months - so much uncertainty ahead

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### WUHAN LIKE

## peak at pat population size peak patients per day
# based on 2.6 / 10,000
## peak remains flat

# Run through the 7 regions of the UK. 
for(ii in 1:length(region_data[,1])){
  print(region_data[ii,1])
  nbeds_reg <- ceiling(region_data[ii,"nccbeds"])
  inc_rate_reg <- ceiling(region_data[ii,"inc_rate"])
  
  cov_curve_reg <- c(sigmoid(c(region_data[ii,"wuhan_covid"],0.3,30),seq(1,90,1)))
  
  M_wuh <- multiple_runs(nruns, nbeds = nbeds_reg, los_norm, los_cov, 
                         cov_curve = cov_curve_reg, ndays, inc_rate = inc_rate_reg)
  
  plot_multiple(M_wuh,paste0("region_",region_data[ii,1]))
  
  # e.g. 
  norm_curve_reg <- rnorm(ndays,inc_rate_reg,1)
  output_eg <- bed_filling(nbeds_reg, los_norm, los_cov, cov_curve_reg,norm_curve_reg,ndays=90)
  plot_eg(output_eg, paste0("region_",region_data[ii,1]), 
          norm_curve = norm_curve_reg, cov_curve = cov_curve_reg)

}

#### Table of outputs

names <- region_data[,1]

cc_store <- c()
ff_store <- c()
gg_store <- c()

for(i in 1:length(names)){
  cc <- read.csv(paste0("outputs/region_",names[i],"_missedpermonth.csv"))[,-1]
  ff <- read.csv(paste0("outputs/region_",names[i],"_extrabed.csv"))[,-1]
  gg <- read.csv(paste0("outputs/region_",names[i],"_totalmissed.csv"))[,-1]
  
  cc_store <- rbind(cc_store, cbind(names[i],cc))
  ff_store <- rbind(ff_store, c(names[i],ff))
  gg_store <- rbind(gg_store, c(names[i],gg))
}

cc_store <- as.data.frame(cc_store)
ff_store <- as.data.frame(ff_store)
gg_store <- as.data.frame(gg_store)
colnames(cc_store) <- c("setting","month","mean_norm","mean_covid")
colnames(ff_store) <- c("setting","mean_extra","sd_extra")
colnames(gg_store) <- c("setting","mean_total_norm","sd_total_norm", "mean_total_covid","sd_total_covid")

ff_store$perc_current <- round(100*ff_store$mean_extra/region_data$nccbeds,0)
