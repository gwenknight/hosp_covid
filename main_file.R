#### Impact of COVID19 on hospital bed capacity 

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

## DATA CALCULATIONS
## Admission rate for "normal" patients
# 180 days (6mo)
# BASED ON ICHNT
# Length of stay 7.9 days
# 157 ADULT beds
# 82.2% full 
occ_bed_days <- c(0.7,0.822,0.9) * 157 * 180  # number of occupied bed days
unique_pat <- occ_bed_days / 7.9 
inc_rate0 <- unique_pat / 180 # number arrive every day
inc_rate0 # mean and range - make norm around mean + 1 SD: 16.3

pop_ichnt <- 560600
ocbeds_peak_wuhan <- 2.6/10000*pop_ichnt# from Lipsitch - but this is number in beds 
ocbeds_peak_wuhan / 10

age_data <- read.csv("data/age_data.csv")

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************

#### NUMBERS
nbeds <- 157 # ICHNT Critical care beds
#nbeds <- 4000 # UK ICU

los_norm <- c(7.9,4) # ICHNT ICU NORMAL LOS 

los_cov <- readRDS("delay_dist_linelist.rds") #sample(seq(1,length(x),1), size = n, prob = x, replace=T) #c(10,8) # GUESS 

cols <- c("3" = "lightblue", "1" = "red", "0" = "darkgreen")

nruns = 100

ndays = 90 # run for 3 months - so much uncertainty ahead

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### WUHAN LIKE
## peak at 15 patients per day
## peak remains flat
cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
#plot(cov_curve)

M_wuh <- multiple_runs(nruns, nbeds = 157, los_norm, los_cov, cov_curve, ndays, inc_rate = 16.3)
plot_multiple(M_wuh,"wuh_ichnt")
write.csv(M_wuh$bed_store,"outputs/wuh_ichnt.csv")

cov_curve <- c(sigmoid(c(12,0.3,30),seq(1,90,1)))
M_wuhd <- multiple_runs(nruns, nbeds = 18, los_norm, los_cov, cov_curve, ndays, inc_rate = 1)
plot_multiple(M_wuhd,"wuh_devon")
write.csv(M_wuhd$bed_store,"outputs/wuh_devon.csv")

cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
M_wuhc <- multiple_runs(nruns, nbeds = 64, los_norm, los_cov, cov_curve, ndays,inc_rate = 7)
plot_multiple(M_wuhc,"wuh_cuh")
write.csv(M_wuhc$bed_store,"outputs/wuh_cuh.csv")

# EG
norm_curve <- rnorm(ndays,16.3,1)
output_wuh <- bed_filling(nbeds, los_norm, los_cov, cov_curve,norm_curve, ndays=90)
plot_eg(output_wuh, "wuh_ichnt", norm_curve, cov_curve)

norm_curve <- rnorm(ndays,1,1)
cov_curve <- c(sigmoid(c(12,0.3,30),seq(1,90,1)))
output_wuh_devon <- bed_filling(18, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_wuh_devon, "wuh_devon", norm_curve, cov_curve)

cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
norm_curve <- rnorm(ndays,7,1)
output_wuh_cuh <- bed_filling(64, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_wuh_cuh, "wuh_cuh", norm_curve, cov_curve)


####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### DOUBLE PEAK HEIGHT OF WUHAN
## peak at 15 patients per day
## peak remains flat 
cov_curve <- c(sigmoid(c(30,0.3,30),seq(1,90,1)))
#plot(cov_curve)
#lines(sigmoid(c(15,0.3,30),seq(1,90,1)))

M_dbwuh <- multiple_runs(nruns, 157, los_norm, los_cov, cov_curve, ndays, inc_rate = 16.3)
plot_multiple(M_dbwuh,"dbwuh_ichnt")
write.csv(M_dbwuh$bed_store,"outputs/dbwuh_ichnt.csv")

cov_curve <- c(sigmoid(c(24,0.3,30),seq(1,90,1)))
M_dbdev <- multiple_runs(nruns, 18, los_norm, los_cov, cov_curve, ndays,inc_rate = 1)
plot_multiple(M_dbdev,"dbwuh_devon")
write.csv(M_dbdev$bed_store,"outputs/dbwuh_devon.csv")

cov_curve <- c(sigmoid(c(30,0.3,30),seq(1,90,1)))
M_dbcuh <- multiple_runs(nruns, 64, los_norm, los_cov, cov_curve, ndays,inc_rate = 7)
plot_multiple(M_dbcuh,"dbwuh_cuh")
write.csv(M_dbcuh$bed_store,"outputs/dbwuh_cuh.csv")

# EG
norm_curve <- rnorm(ndays,16.3,1)
cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
output_dbwuh <- bed_filling(157, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_dbwuh, "dbwuh_ichnt", norm_curve, cov_curve)

norm_curve <- rnorm(ndays,1,1)
cov_curve <- c(sigmoid(c(24,0.3,30),seq(1,90,1)))
output_dbwuh_devon <- bed_filling(18, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_dbwuh_devon, "dbwuh_devon", norm_curve, cov_curve)

norm_curve <- rnorm(ndays,7,1)
cov_curve <- c(sigmoid(c(30,0.3,30),seq(1,90,1)))
output_dbwuh_cuh <- bed_filling(64, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_dbwuh_cuh, "dbwuh_cuh", norm_curve, cov_curve)


####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### HALVE RATE OF WUHAN
## peak at 15 patients per day
## peak remains flat 
## Rate to increase halved
cov_curve <- c(sigmoid(c(15,0.15,45),seq(1,90,1)))

pdf("plots/scenarios_covid.pdf")
plot(cov_curve,ylim=c(0,35),type="l",col="blue",xlab = "Days",ylab = "Daily incidence of COVID19 critical care cases")
lines(sigmoid(c(15,0.3,30),seq(1,90,1)),col="black")
lines(sigmoid(c(30,0.3,30),seq(1,90,1)),col="red")
abline(v=c(30,60,90),col="grey")
abline(h=16.3,col="black",lty="dashed")
dev.off()

M_slwuh <- multiple_runs(nruns, nbeds, los_norm, los_cov, cov_curve, ndays, inc_rate = 16.3)
plot_multiple(M_slwuh,"slwuh_ichnt")
write.csv(M_slwuh$bed_store,"outputs/slwuh_ichnt.csv")

cov_curve <- c(sigmoid(c(12,0.15,45),seq(1,90,1)))
M_sldev <- multiple_runs(nruns, 18, los_norm, los_cov, cov_curve, ndays,inc_rate = 1)
plot_multiple(M_sldev,"slwuh_devon")
write.csv(M_sldev$bed_store,"outputs/slwuh_devon.csv")

cov_curve <- c(sigmoid(c(15,0.15,45),seq(1,90,1)))
M_slcuh <- multiple_runs(nruns, 64, los_norm, los_cov, cov_curve, ndays,inc_rate = 7)
plot_multiple(M_slcuh,"slwuh_cuh")
write.csv(M_slcuh$bed_store,"outputs/slwuh_cuh.csv")

# EG
norm_curve <- rnorm(ndays,16.3,1)
cov_curve <- c(sigmoid(c(15,0.3,45),seq(1,90,1)))
output_slwuh <- bed_filling(nbeds, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_slwuh, "slwuh_ichnt", norm_curve, cov_curve)

norm_curve <- rnorm(ndays,1,1)
cov_curve <- c(sigmoid(c(12,0.15,45),seq(1,90,1)))
output_sldevon <- bed_filling(18, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_sldevon, "slwuh_devon", norm_curve, cov_curve)

norm_curve <- rnorm(ndays,7,1)
cov_curve <- c(sigmoid(c(15,0.15,45),seq(1,90,1)))
output_slcuh <- bed_filling(64, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_slcuh, "slwuh_cuh", norm_curve, cov_curve)




#############**********************************************************************
#############**********************************************************************
#############**********************************************************************
#############**********************************************************************
## What if reduce "normal" ICU burden by 46%
# based on https://onlinelibrary.wiley.com/doi/full/10.1111/j.1365-2044.2009.06084.x

### WUHAN LIKE
## peak at 15 patients per day
## peak remains flat
cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
#plot(cov_curve)

M_wuh2 <- multiple_runs(nruns, nbeds = 157, los_norm, los_cov, cov_curve, ndays, inc_rate = 16.3/2)
plot_multiple(M_wuh2,"halfn_wuh_ichnt")
write.csv(M_wuh2$bed_store,"outputs/halfn_wuh_ichnt.csv")

cov_curve <- c(sigmoid(c(12,0.3,30),seq(1,90,1)))
M_wuhd2 <- multiple_runs(nruns, nbeds = 18, los_norm, los_cov, cov_curve, ndays, inc_rate = 1/2)
plot_multiple(M_wuhd2,"halfn_wuh_devon")
write.csv(M_wuhd2$bed_store,"outputs/halfn_wuh_devon.csv")

cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
M_wuhc2 <- multiple_runs(nruns, nbeds = 64, los_norm, los_cov, cov_curve, ndays,inc_rate = 7/2)
plot_multiple(M_wuhc2,"halfn_wuh_cuh")
write.csv(M_wuhc2$bed_store,"outputs/halfn_wuh_cuh.csv")

# EG
norm_curve <- rnorm(ndays,16.3/2,1)
cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
output_wuh2 <- bed_filling(nbeds, los_norm, los_cov, cov_curve,norm_curve, ndays=90)
plot_eg(output_wuh2, "halfn_wuh_ichnt", norm_curve, cov_curve)

norm_curve <- rnorm(ndays,1/2,1)
cov_curve <- c(sigmoid(c(12,0.3,30),seq(1,90,1)))
output_wuh_devon2 <- bed_filling(18, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_wuh_devon2, "halfn_wuh_devon",norm_curve, cov_curve)

norm_curve <- rnorm(ndays,7/2,1)
cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
output_wuh_cuh2 <- bed_filling(64, los_norm, los_cov, cov_curve,norm_curve,ndays=90)
plot_eg(output_wuh_cuh2, "halfn_wuh_cuh", norm_curve, cov_curve)

#### Table of outputs

names <- c("wuh_ichnt","wuh_devon","wuh_cuh",
           "dbwuh_ichnt","dbwuh_devon","dbwuh_cuh",
           "slwuh_ichnt","slwuh_devon","slwuh_cuh",
           "halfn_wuh_ichnt","halfn_wuh_devon","halfn_wuh_cuh")

cc_store <- c()
ff_store <- c()
gg_store <- c()

for(i in 1:length(names)){
  cc <- read.csv(paste0("outputs/",names[i],"_missedpermonth.csv"))[,-1]
  ff <- read.csv(paste0("outputs/",names[i],"_extrabed.csv"))[,-1]
  gg <- read.csv(paste0("outputs/",names[i],"_totalmissed.csv"))[,-1]
  
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

##### How many cases to NOT BREAK NHS
# Could do 



