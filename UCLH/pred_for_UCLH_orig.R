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
#los_norm <- c(7.9,4) # ICHNT ICU NORMAL LOS 
# Assume just A&E and cancer emergency patients. 
# A&E dist: Gamma(0.8620528943394684, 6.4821837628800365)
# cancer: Gamma(1.1539178139996413, 2.5241128439179894)
prop_ae <- 0.5
prop_cancer <- 0.5
x <- rgamma(prop_ae*100000, shape = 0.862, scale = 6.48) + rgamma(prop_cancer*100000, shape = 1.15, scale = 2.52)
fit.gamma <- fitdist(x, distr = "gamma", method = "mle")
#summary(fit.gamma)
#plot(fit.gamma)
los_norm_icu <- c(1, fit.gamma$estimate["shape"], fit.gamma$estimate["rate"]) ## GAMMA
los_cov <- readRDS("delay_dist_linelist.rds") #sample(seq(1,length(x),1), size = n, prob = x, replace=T) #c(10,8) # GUESS 

cols <- c("3" = "lightblue", "1" = "red", "0" = "darkgreen")

nruns = 100

ndays = 17 # run for 2 weeks

nbeds_uclh <- 61
occ_bed_days_uclh <- 0.754 * nbeds_uclh # occupied beds
inc_rate_uclh    <- ceiling(occ_bed_days_uclh / 8.6)

pdischarge <- c()
pdischarge$covid <- 0.5
pdischarge$normal <- 1-0.083 # 8.3 die from HES

#### PLOTS of inputs
los_norm_icu_examples <- rgamma(10000, fit.gamma$estimate["shape"], fit.gamma$estimate["rate"])
pdf("los_norm_uclh.pdf")
h <- hist(los_norm_icu_examples, breaks = seq(0,50,1))
dev.off()


####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
####****************************************************************************************************************
### HIGH
cov_curve_high <- c(rep(0,3),exp(0.18*seq(1,14,1)))
plot(cov_curve_high)

norm_curve_uclh_half <- c(rep(inc_rate_uclh,3),seq(1,0.5,length.out = 14)*inc_rate_uclh) # flat then covid

M_high <- multiple_runs(nruns, nbeds = nbeds_uclh, los_norm_icu, los_cov, 
                       cov_curve_high, ndays, inc_rate = norm_curve_uclh_half,pdischarge)

plot_multiple(M_high,paste0("UCLH_high"))

# e.g. 
#norm_curve_uclh <- rnorm(ndays,inc_rate_uclh,1)
output_eg <- bed_filling(nbeds_uclh, los_norm_icu, los_cov, cov_curve_high,norm_curve_uclh_half,ndays=17,pdischarge)
plot_eg(output_eg, paste0("UCLH_high"),norm_curve_uclh_half, cov_curve_high)


### LOW
cov_curve_low <- c(rep(0,3),exp(0.02*seq(1,14,1)))
plot(cov_curve_low)

M_low <- multiple_runs(nruns, nbeds = nbeds_uclh, los_norm_icu, los_cov, 
                       cov_curve_low, ndays, inc_rate = norm_curve_uclh_half,pdischarge)

plot_multiple(M_low,paste0("UCLH_low"))

# e.g. 
norm_curve_uclh <- rnorm(ndays,inc_rate_uclh,1)
output_eg <- bed_filling(nbeds_uclh, los_norm_icu, los_cov, cov_curve_low,norm_curve_uclh_half,ndays=17,pdischarge)
plot_eg(output_eg, paste0("UCLH_low"),norm_curve_uclh_half, cov_curve_low)

### MEDIUM
cov_curve_med <- c(rep(0,3),exp(0.1*seq(1,14,1)))
plot(cov_curve_med)

M_med <- multiple_runs(nruns, nbeds = nbeds_uclh, los_norm_icu, los_cov, 
                       cov_curve_med, ndays, inc_rate = norm_curve_uclh_half,pdischarge)

plot_multiple(M_med,paste0("UCLH_med"))

# e.g. 
norm_curve_uclh <- rnorm(ndays,inc_rate_uclh,1)
output_eg <- bed_filling(nbeds_uclh, los_norm_icu, los_cov, cov_curve_med,norm_curve_uclh_half,ndays=17,pdischarge)
plot_eg(output_eg, paste0("UCLH_med"),norm_curve_uclh_half, cov_curve_med)


#####**********
# Scenario plot
scen <- as.data.frame(cbind(seq(1,ndays,1),norm_curve_uclh_half, cov_curve_high, cov_curve_low,cov_curve_med))
colnames(scen) <- c("time","norm","cov_high","cov_low","cov_med")
scenm <- melt(scen, id.vars = "time")

ggplot(scenm, aes(x=time, y = value)) + geom_line(aes(group = variable, col = variable)) + 
  scale_x_continuous("Day") + scale_y_continuous("Daily admission need") +
  scale_colour_discrete(name ="Scenario",labels=c("Non-COVID patient","COVID High","COVID Low","COVID Medium")) +
  geom_hline(yintercept = inc_rate_uclh, lty = "dotted") + theme_bw()
ggsave("plots/scenarios_uclh.pdf")

#####***** PLOT together
M_all_m <- as.data.frame(cbind(M_high$total_beds_time[,c("time","mbed")],
               M_med$total_beds_time[,c("mbed")],
               M_low$total_beds_time[,c("mbed")]))
colnames(M_all_m) <- c("time","high","med","low")
M_all_mean <- melt(M_all_m, id.vars = "time") 
M_all_mean$mean.need <- M_all_mean$value

M_all_sd <- cbind(M_high$total_beds_time[,c("time","sdbed")],
                 M_med$total_beds_time[,c("sdbed")],
                M_low$total_beds_time[,c("sdbed")])
colnames(M_all_sd) <- c("time","high","med","low")
colnames(M_all_sd) <- c("time","high","med","low")
M_all_sd <- melt(M_all_sd, id.vars = "time") 
M_all_sd$sd.need <- M_all_sd$value

M_all <- cbind(M_all_mean[,c("time","variable","mean.need")], M_all_sd$sd.need)
colnames(M_all) <- c("time","variable","mean.need","sd.need")

ggplot(M_all,aes(x=time, y = mean.need, group = variable)) + 
  geom_ribbon(aes(group = variable, ymin = mean.need - sd.need, ymax = mean.need +sd.need, fill = variable), alpha = 0.4) + 
  geom_line(aes(y = mean.need)) + 
  scale_fill_discrete("Scenario") + 
  scale_y_continuous("Number of beds needed") + 
  scale_x_continuous("Days",lim = c(3,17)) + 
  geom_hline(yintercept = nbeds, col = "red",lty = "dashed") + theme_bw()

ggsave(paste0("plots/bed_need_overtime_","UCLH",".pdf"))


#### Table of outputs

names <- c("UCLH_low","UCLH_med","UCLH_high")

cc_store <- c()
ff_store <- c()
gg_store <- c()

for(i in 1:length(names)){
  #cc <- read.csv(paste0("outputs/",names[i],"_missedpermonth.csv"))[,-1]
  ff <- read.csv(paste0("outputs/",names[i],"_extrabed.csv"))[,-1]
  gg <- read.csv(paste0("outputs/",names[i],"_totalmissed.csv"))[,-1]
  
  #cc_store <- rbind(cc_store, cbind(names[i],cc))
 ff_store <- rbind(ff_store, c(ff))
  gg_store <- rbind(gg_store, c(gg))
}

#cc_store <- as.data.frame(cc_store)
ff_store <- as.data.frame(ff_store)
gg_store <- as.data.frame(gg_store)
#colnames(cc_store) <- c("setting","month","mean_norm","mean_covid")
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
write.csv(results_tab,"outputs/table_results_UCLH.csv")      
      








