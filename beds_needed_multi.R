#############**********************************************************************
#############**********************************************************************
#############**********************************************************************
#############**********************************************************************
##### Beds needed multipanel plot

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

# EG
norm_curve <- rnorm(ndays,16.3,1)
output_wuh <- bed_filling(nbeds, los_norm, los_cov, cov_curve,norm_curve, ndays=90)


norm_curve <- rnorm(ndays,1,1)
cov_curve <- c(sigmoid(c(12,0.3,30),seq(1,90,1)))
output_wuh_devon <- bed_filling(18, los_norm, los_cov, cov_curve,norm_curve,ndays=90)


norm_curve <- rnorm(ndays,7,1)
cov_curve <- c(sigmoid(c(15,0.3,30),seq(1,90,1)))
output_wuh_cuh <- bed_filling(64, los_norm, los_cov, cov_curve,norm_curve,ndays=90)


####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### DOUBLE PEAK HEIGHT OF WUHAN
## peak at 15 patients per day
## peak remains flat 
cov_curve <- c(sigmoid(c(30,0.3,30),seq(1,90,1)))


# EG
norm_curve <- rnorm(ndays,16.3,1)
output_dbwuh <- bed_filling(157, los_norm, los_cov, cov_curve,norm_curve,ndays=90)

norm_curve <- rnorm(ndays,1,1)
cov_curve <- c(sigmoid(c(24,0.3,30),seq(1,90,1)))
output_dbwuh_devon <- bed_filling(18, los_norm, los_cov, cov_curve,norm_curve,ndays=90)

norm_curve <- rnorm(ndays,7,1)
cov_curve <- c(sigmoid(c(30,0.3,30),seq(1,90,1)))
output_dbwuh_cuh <- bed_filling(64, los_norm, los_cov, cov_curve,norm_curve,ndays=90)

####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### HALVE RATE OF WUHAN
## peak at 15 patients per day
## peak remains flat 
## Rate to increase halved
cov_curve <- c(sigmoid(c(15,0.15,45),seq(1,90,1)))

# EG
norm_curve <- rnorm(ndays,16.3,1)
output_slwuh <- bed_filling(nbeds, los_norm, los_cov, cov_curve,norm_curve,ndays=90)

norm_curve <- rnorm(ndays,1,1)
cov_curve <- c(sigmoid(c(12,0.15,45),seq(1,90,1)))
output_sldevon <- bed_filling(18, los_norm, los_cov, cov_curve,norm_curve,ndays=90)

norm_curve <- rnorm(ndays,7,1)
cov_curve <- c(sigmoid(c(15,0.15,45),seq(1,90,1)))
output_slcuh <- bed_filling(64, los_norm, los_cov, cov_curve,norm_curve,ndays=90)

####

extrabed_plot <- function(outputnn, ){
  
  Mm <- outputnn
  w<-which(Mm$status != 3)
  mm <- as.data.frame(Mm[w,])
  
  mm_min <- mm %>% group_by(j, bedno) %>% slice(which.min(time))
  g <- ggplot(mm_min,aes(x=time, y = bedno)) + geom_point(aes(col=j)) + ## NEED just first
    scale_y_continuous("Number of beds needed") + 
    scale_color_continuous(guide=FALSE) + 
    scale_x_continuous("Days")
  
}

####
g1 <- extrabed_plot(M_wuh$bed_store)
g2 <- extrabed_plot(M_wuhd$bed_store)
g3 <- extrabed_plot(M_wuhc$bed_store)
g4 <- extrabed_plot(M_dbwuh$bed_store)
g5 <- extrabed_plot(M_dbdev$bed_store)
g6 <- extrabed_plot(M_dbcuh$bed_store)
g7 <- extrabed_plot(M_slwuh$bed_store)
g8 <- extrabed_plot(M_sldev$bed_store)
g9 <- extrabed_plot(M_slcuh$bed_store)

(g1 | g2 | g3)/(g4 | g5 | g6)/(g7 | g8 | g9)
ggsave("plots/timing_extra_beds.pdf")
## bed_store has data - change to a curve

