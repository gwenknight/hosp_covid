####### PREDICTIONS FROM CHESS

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

nruns = 10

ndays = 14 # run for 2 weeks

nbeds_eng <- 4048
occ_bed_days_eng <- 0.734 * nbeds_eng # occupied beds
inc_rate_eng    <- ceiling(occ_bed_days_eng / 7.9)

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### CHESS LIKE
cov_curve <- 1 + exp(0.2*seq(1,28,1))[15:28]
plot(cov_curve)

M_wuh <- multiple_runs(nruns, nbeds = nbeds_uk, los_norm, los_cov, 
                       cov_curve, ndays, inc_rate = inc_rate_uk)

plot_multiple(M_wuh,paste0("England_",region_data[ii,1]))

# e.g. 
norm_curve_reg <- rnorm(ndays,inc_rate_eng,1)
output_eg <- bed_filling(nbeds_eng, los_norm, los_cov, cov_curve,norm_curve,ndays=14)
plot_eg(output_eg, paste0("England_"),norm_curve, cov_curve)

}
