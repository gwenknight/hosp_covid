####### PREDICTIONS FROM UCLH

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

#### NUMBERS
los_norm <- c(7.9,4) # ICHNT ICU NORMAL LOS 
los_cov <- readRDS("delay_dist_linelist.rds") #sample(seq(1,length(x),1), size = n, prob = x, replace=T) #c(10,8) # GUESS 

cols <- c("3" = "lightblue", "1" = "red", "0" = "darkgreen")

nruns = 100

ndays = 14 # run for 2 weeks

nbeds_uclh <- 61
occ_bed_days_uclh <- 0.754 * nbeds_uclh # occupied beds
inc_rate_uclh    <- ceiling(occ_bed_days_uclh / 7.9)

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### HIGH
cov_curve_high <- round(exp(0.3*seq(1,28,1))[15:28]*0.02,0) # 
plot(cov_curve_high)

M_wuh <- multiple_runs(nruns, nbeds = nbeds_uclh, los_norm, los_cov, 
                       cov_curve_high, ndays, inc_rate = inc_rate_uclh)

plot_multiple(M_wuh,paste0("UCLH_high"))

# e.g. 
norm_curve_uclh <- rnorm(ndays,inc_rate_uclh,1)
output_eg <- bed_filling(nbeds_uclh, los_norm, los_cov, cov_curve_high,norm_curve_uclh,ndays=14)
plot_eg(output_eg, paste0("UCLH_high"),norm_curve_uclh, cov_curve_high)


### LOW
cov_curve_low <- round(exp(0.24*seq(1,28,1))[15:28]*0.02,0) # 
plot(cov_curve_low)

M_wuh <- multiple_runs(nruns, nbeds = nbeds_uclh, los_norm, los_cov, 
                       cov_curve_low, ndays, inc_rate = inc_rate_uclh)

plot_multiple(M_wuh,paste0("UCLH_low"))

# e.g. 
norm_curve_uclh <- rnorm(ndays,inc_rate_uclh,1)
output_eg <- bed_filling(nbeds_uclh, los_norm, los_cov, cov_curve_low,norm_curve_uclh,ndays=14)
plot_eg(output_eg, paste0("UCLH_low"),norm_curve_uclh, cov_curve_low)

### LOW plus reduced incoming normal patients
cov_curve_low <- round(exp(0.24*seq(1,28,1))[15:28]*0.02,0) # 
plot(cov_curve_low)

M_wuh <- multiple_runs(nruns, nbeds = nbeds_uclh, los_norm, los_cov, 
                       cov_curve_low, ndays, inc_rate = seq(1,0.5,length.out = length(cov_curve_low))*inc_rate_uclh)

plot_multiple(M_wuh,paste0("UCLH_low"))

# e.g. 
norm_curve_uclh <- rnorm(ndays,inc_rate_uclh,1)
output_eg <- bed_filling(nbeds_uclh, los_norm, los_cov, cov_curve_low,norm_curve_uclh,ndays=14)
plot_eg(output_eg, paste0("UCLH_low"),norm_curve_uclh, cov_curve_low)
