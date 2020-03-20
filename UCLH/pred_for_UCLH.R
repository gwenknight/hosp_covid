####### PREDICTIONS FOR UCLH

library(ggplot2)
library(reshape2)
library(patchwork)
library(gridGraphics)
library(tidyverse)
library(fitdistrplus)

#### FUNCTIONS
source("functions_uclh_covid.R")


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

#### QUERY COVID PARMETERS
paraq <- c()
paraq$qn <- 0.1 # 10% N
paraq$qc <- 0.95 # most covid

### Number of beds
nbeds_sr = 10
los_norm_sr = 1  # normal dist parameters for this?
los_cov_sr = 1 # normal dist paramters for this?

## Admissions to ICU ?
icu_norm_rate = 0.1 #?
icu_cov_rate = 0.1 # ?

#### SIZES
nbeds_standard = 1000 
nbeds_covid = 1000
nbeds_icu = 61

# STANDARD WARD
los_norm_standard <- c(7.9,4) # ICHNT ICU NORMAL LOS
los_cov_standard <- c(0,0) # none in there
para_standard <- c()
para_standard$p_discharge_covid <- 0 # none there
para_standard$p_discharge_norm <- 

# COV WARD
los_norm_cov <- c(0,0) # none in there
los_cov_cov <- c()
para_cov <- c()
para_cov$p_discharge_covid <- 
para_cov$p_discharge_norm <- 0 # none in there

# ICU WARD
# Assume just A&E and cancer emergency patients. 
  # A&E dist: Gamma(0.8620528943394684, 6.4821837628800365)
  # cancer: Gamma(1.1539178139996413, 2.5241128439179894)
prop_ae <- 0.5
prop_cancer <- 0.5
x <- rgamma(prop_ae*100000, shape = 0.862, scale = 6.48) + rgamma(prop_cancer*100000, shape = 1.15, scale = 2.52)
fit.gamma <- fitdist(x, distr = "gamma", method = "mle")
#summary(fit.gamma)
#plot(fit.gamma)
los_norm_icu <- c(fit.gamma$estimate["shape"], fit.gamma$estimate["rate"]) ## GAMMA

los_cov_icu <- readRDS("delay_dist_linelist.rds")

para_icu <- c()
para_icu$p_discharge_covid <- 1 - 0.78
para_icu$p_discharge_norm <- 

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### HIGH
cov_curve_high <- round(exp(0.3*seq(1,28,1))[15:28]*0.02,0) # 
norm_curve <- round(rnorm(ndays_i,inc_rate_uclh,1),0)

### From above patients can enter side rooms
side_room_calcs <- admin_ed_query_side_room(cov_curve, norm_curve, paraq)

A_non_sr <- side_room_calcs$A
SR <- side_room_calcs$SR

### SIDE ROOM FILLING
# SR_fill <- bed_filling_uclh(SR, nbeds_sr, los_norm_sr, los_cov_sr,para) # IF GET LOS_NORM in SIDE ROOM
# ASSUME LOS JUST ONE DAY
SR$norm_sr[2:ndays] <- SR$norm_sr[1:(ndays-1)]
SR$cov_sr[2:ndays] <- SR$cov_sr[1:(ndays-1)]


### ADMISSIONS TO STANDARD BEDS
A_standard_rooms <- A_non_sr
A_standard_rooms$cov_admin <- 0 # ASSUME NO COVID INTO STANDARD
A_standard_rooms$norm_admin <- A_standard_rooms$norm_admin*(1-icu_norm_rate)

## ADMISSIONS TO COVID BEDS
A_cov_beds <- A_non_sr
A_cov_beds$norm_admin <- 0 # ASSUME NO NORMAL INTO COVID

## COMPETITION 
A_icu_beds <- A_non_sr
A_icu_beds$norm_admin <- A_non_sr*icu_norm_rate # ICU FROM NON SR
A_icu_beds$cov_admin  <- A_non_sr*icu_cov_rate # ICU FROM NON SR

# ASSUMES 1 days in SR
A_icu_beds$norm_admin[2:ndays] <- A_icu_beds$norm_admin[2:ndays] + SR$norm_sr[1:(ndays-1)] # ICU FROM SR
A_icu_beds$cov_admin[2:ndays]  <- A_icu_beds$cov_admin[2:ndays]  + SR$cov_sr[1:(ndays-1)] # ICU FROM SR

## FILL BEDS
M_standard_beds<- bed_filling_uclh(A_standard_rooms, nbeds_standard, los_norm_standard, los_cov_standard, para_standard)
M_cov_beds <- bed_filling_uclh(A_cov_rooms, nbeds_cov, los_norm_cov, los_cov_cov, para_cov)
M_icu_beds <- bed_filling_uclh(A_icu_rooms, nbeds_icu, los_norm_icu, los_cov_icu, para_icu)




