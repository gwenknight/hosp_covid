## FUNCTIONS
### Fills beds for 6 months. Daily time step
# nbeds = number of beds in ward
# los = length of stay (mean, sd) for normal / covid patients
# cov_curve = changing NUMBER of patients that are covid+
# inc_rate = ICNHT data

bed_filling <- function(nbeds, los_norm, los_cov, cov_curve, norm_curve, ndays = 90){
  
  ## Admissions 
  A <- as.data.frame(matrix(0,ndays,4))
  colnames(A) <- c("day","norm_admin","cov_admin","prop_cov")
  A[,"day"] <- seq(1,ndays,1)
  A[,"norm_admin"] <- ceiling(norm_curve) #ceiling(rnorm(ndays,inc_rate,1)) #  depends on normal number of bed days / LOS of hospital
  A[,"cov_admin"] <- ceiling(cov_curve)
  A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin)
  A0 <- A
  
  #CRITICAL BED DAYS
  WC <- array(0,c(nbeds,2,(ndays+1))) # Array - 3D matrix. 
  colnames(WC)<-c("patno","status")
  #NEW BEDS NEEDED
  WN <- array(0,c(5000,2,(ndays+1))) # Array - 3D matrix. 
  colnames(WN)<-c("patno","status")
  # rows = bed, columns = c(patient number, actual status, presumed status, days in hospital), 3D = time
  
  # track number of patients 
  pat_num <- 0
  
  ###############**********************************************************************
  ### ICU BEDS ###
  ###############**********************************************************************
  
  ICU_fill <- as.data.frame(matrix(0,ndays,3)) # Want to know how many enter the ICU each day
  colnames(ICU_fill) <- c("day","norm","cov")
  ICU_fill$day <- seq(1,ndays,1)
  
  # fill by bed
  for(i in 1:nbeds){
    
    cumlos <- 0 # DAY ZERO
    
    # first patient in bed
    pat_num <- pat_num + 1
    if(runif(1) < 0.2){ # 20% empty beds
      pat_status <- 3 # Empty bed
      los <- 1 # check next day for patient
    }else{
      pat_status <- 0 # normal patient in initially - assume v low prevalence of COVID
      los <- ceiling(rnorm(1,los_norm) * runif(1)) # been in for some time already perhaps
      ICU_fill[1,(pat_status+2)] <- ICU_fill[1,(pat_status+2)] + 1
    }
    
    WC[i,c("patno","status"),1:los] <- c(pat_num,pat_status)
    
    cumlos <- cumlos + los
    
    while(cumlos < (ndays+1)){
      #print(cumlos)
      pat_num <- pat_num + 1 # Next patient
      ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
      if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
        #print(c(cumlos,pat_status))
        pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
        
        if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted
          
          ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          
          los <- ceiling(ifelse(pat_status == 1, 
                                sample(los_cov, 1, replace = TRUE), #rnorm(1,los_cov), 
                                rnorm(1,los_norm))) # length of stay
          
        }else{
          pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
          # don't need to check if patient of this type as know sum (A) > 0
          
          ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,
                                         A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          
          
          los <- ceiling(ifelse(pat_status == 1, 
                                sample(los_cov, 10, replace = TRUE),#rnorm(1,los_cov), 
                                rnorm(1,los_norm))) # length of stay
          
        }
      }else{
        los <- 1
        pat_status <- 3 # EMPTY BED
        pat_num <- pat_num - 1 # No patient
      }
      
      # If empty bed don't store patient number 
      if(pat_status == 3){
        WC[i,c("patno","status"),pmin((cumlos + (1:los)),ndays)] <- c(0,pat_status) # this patient stays until end of los
      }else{
        WC[i,c("patno","status"),pmin((cumlos + (1:los)),ndays)] <- c(pat_num,pat_status) # this patient stays until end of los
      }
      
      cumlos <- cumlos + los # next new patient at this time point
    }
    
  }
  Aleft <- A
  
  i = 1
  #print(pat_num)
  # Extra beds needed
  while( sum(A$norm_admin + A$cov_admin) > 0){
    
    cumlos <- 1
    WN[i,c("patno","status"),1] <- c(pat_num+1,3) # First day empty. A > 0 so will have a new patient to admit but not here and now
    
    while(cumlos < (ndays + 1)){
      #print(c("1",cumlos, pat_num,pat_status))
      pat_num <- pat_num + 1 # Next patient
      #  print(c("2",cumlos, pat_num,pat_status))
      ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
      if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
        #print(c(cumlos,pat_status))
        
        pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
        
        if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted on that day
          
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          los <- ceiling(ifelse(pat_status == 1, 
                                sample(los_cov, 1, replace = TRUE), #rnorm(1,los_cov), 
                                rnorm(1,los_norm))) # length of stay
          
        }else{
          pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
          # don't need to check if patient of this type as know sum (A) > 0
          
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,
                                         A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          los <- ceiling(ifelse(pat_status == 1, 
                                sample(los_cov, 10, replace = TRUE),#rnorm(1,los_cov), 
                                rnorm(1,los_norm))) # length of stay
        }
      }else{
        los <- 1
        pat_status <- 3 # EMPTY BED
        pat_num <- pat_num - 1 # No patient
      }
      # print(c("3",cumlos, pat_num,pat_status))
      # If empty bed don't store patient number 
      
      if(pat_status == 3){
        WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays)] <- c(0,pat_status) # this patient stays until end of los
      }else{
        WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays)] <- c(pat_num,pat_status) # this patient stays until end of los
      }
      
      cumlos <- cumlos + los # next new patient at this time point
      # print(c("4",cumlos, pat_num,pat_status))
    }
    i = i + 1 # move on to next bed
    
  }
  
  
  return(list(A0 = A0, A = A, Aleft = Aleft, WC = WC, WN = WN, pat_num = pat_num, ICU_fill = ICU_fill))
  
}

### Multiple runs

multiple_runs <- function(nruns, nbeds, los_norm, los_cov, cov_curve, inc_rate, ndays = 90){
  
  h_store<-c()
  missing_store <- c() #matrix(0,100*ndays,5)
  bed_store <- c()
  max_bed_need <- c()
  miss_norm <- c()
  miss_covi <- c()
  
  for(j in 1:nruns){
    norm_curve <- rnorm(ndays,inc_rate,1)
    output <- bed_filling(nbeds, los_norm, los_cov, cov_curve, norm_curve, ndays)
    
    h <- melt(output$WC,id.vars = "patno")
    colnames(h) <- c("bedno","variable","time","value")
    h <- dcast(h, time + bedno ~variable)
    h <- h[-which(h$time == (ndays + 1)),]
    
    h_store <- rbind(h_store, cbind(j,h))
    missing_store <- rbind(missing_store, cbind(j,output$Aleft))
    
    # Extra beds need 
    n <- melt(output$WN,id.vars = "patno")
    colnames(n) <- c("bedno","variable","time","value")
    n <- dcast(n, time + bedno ~variable)
    n <- n[-which(n$time == (ndays+1)),]
    
    ll <- n %>% group_by(bedno) %>% summarise(mean(patno)) # which beds actually have patients in
    mm <- ifelse(length(which(ll[,2]>0,arr.ind = TRUE))>0,max(which(ll[,2]>0,arr.ind = TRUE)),0) # max bed number
    ww <- which(n$bedno <= mm)
    n <- n[ww,]
    
    # Some of them have enough beds
    if(dim(n)[1] == 0){n <- c(0,0,3)}
    
    bed_store <- rbind(bed_store, cbind(j,n))
    max_bed_need <- c(max_bed_need,mm)
    miss_norm <- c(miss_norm, sum(output$Aleft[,c("norm_admin")]))
    miss_covi <- c(miss_covi, sum(output$Aleft[,c("cov_admin")]))
    
  }
  
  ## Multiple runs
  h_store <- as.data.frame(h_store)
  missing_store <- as.data.frame(missing_store)
  miss <- missing_store %>% group_by(day) %>% 
    summarise(m_norm = mean(norm_admin), m_cov = mean(cov_admin), 
              sd_norm = sd(norm_admin), sd_cov = sd(cov_admin)) 
  
  
  # mean number missed per day
  missing_store$month <- rep(1:(ndays/30),each = 30)
  miss_month <- missing_store %>% group_by(month) %>% 
    summarise(m_norm = mean(norm_admin), m_cov = mean(cov_admin), 
              sd_norm = sd(norm_admin), sd_cov = sd(cov_admin)) 
  
  # mean total missed
  total_missed <- c(mean(miss_norm), sd(miss_norm), 
                    mean(miss_covi), sd(miss_covi))
  
  return(list(missing_store = missing_store, miss_month = miss_month,
              h_store = h_store, miss = miss,
              bed_store = bed_store, max_bed_need = max_bed_need,
              total_missed = total_missed))
  
}

sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}


#### E.G. PLOT

## e.g.

plot_eg <- function(output, name, norm_curve, cov_curve){
  
  # Grab data
  # Basic time data to ndays days
  h <- melt(output$WC,id.vars = "patno")
  colnames(h) <- c("bedno","variable","time","value")
  h <- dcast(h, time + bedno ~variable)
  h <- h[-which(h$time == (ndays+1)),]
  
  # Plot this
  p1 <- ggplot(h, aes(x = time, y = bedno) ) + 
    geom_point(aes(col = factor(status))) + 
    scale_colour_manual(name  ="Status",values = cols,breaks=c("0", "3","1"),labels=c("Normal", "Empty","COVID")) + 
    xlab("Day") + ylab("Bed number") +
    geom_vline(xintercept = c(30,60,90),col="grey",lty = "dashed")
  
  pcov <- as.data.frame(cbind(seq(1,length(cov_curve),1),cov_curve));
  colnames(pcov)<-c("days","cprev")
  pcov$norm <- norm_curve
  pcovm <- melt(pcov, id.vars = "days")
  
  p2 <- ggplot(pcovm, aes(x=days, y = value)) + geom_line(aes(group = variable, col = variable)) + 
    scale_x_continuous("Day") + scale_y_continuous("Daily admission\nneed") +
    geom_vline(xintercept = c(30,60,90),col="grey",lty = "dashed") + 
    scale_colour_manual(name ="Status",values=c("norm"="darkgreen","cprev" = "red"),labels=c("Normal", "COVID")) +
    geom_hline(yintercept = mean(norm_curve), lty = "dotted") + 
    annotate(size = 2,'text',10, round(mean(norm_curve)+2), 
             label=paste("Mean norm. (daily):",round(mean(norm_curve))))
  
  # Missing people
  miss <- melt(output$Aleft[,c("day","norm_admin","cov_admin")], id.vars = "day")
  #perc_not_treat <- round(100*output$pat_num/(output_nocovid$pat_num),0)
  
  p3 <- ggplot(miss, aes(fill=variable, y=value, x=day)) + 
    geom_bar(position="stack", stat="identity") + 
    scale_fill_manual(name  ="Status",values=c("norm_admin"="darkgreen","cov_admin" = "red"),labels=c("Normal", "COVID")) +
    scale_y_continuous("Extra bed space\nneeded per day") + 
    scale_x_continuous("Day") + 
    #annotate(size = 2,'text',90, 20, 
    # label=paste("Percentage of normal ICU burden treated:",perc_not_treat,"%"))+ 
    annotate(size = 2,'text',10, 19, 
             label=paste("Total missed norm:",sum(output$Aleft[,c("norm_admin")]))) +
    annotate(size = 2,'text',10, 15, 
             label=paste("Total missed covid:",sum(output$Aleft[,c("cov_admin")]))) +
    geom_vline(xintercept = c(30,60,90),col="grey",lty = "dashed")
  
  
  p2/p1+p3
  ggsave(paste0("plots/eg_",name,".pdf"))
  
  ### Beds needed
  n <- melt(output$WN,id.vars = "patno")
  colnames(n) <- c("bedno","variable","time","value")
  n <- dcast(n, time + bedno ~variable)
  n <- n[-which(n$time == (ndays+1)),]
  
  ll <- n %>% group_by(bedno) %>% summarise(mean(patno)) # which beds actually have patients in
  mm <- max(which(ll[,2]>0,arr.ind = TRUE)) # max bed number
  ww <- which(n$bedno <= mm)
  n <- n[ww,]
  
  ymax = ifelse(mm > 500,5000,250)
    
  g <- ggplot(n, aes(x = time, y = bedno) ) + 
    geom_point(aes(col = factor(status))) + 
    scale_colour_manual(name  ="Status",values = cols,breaks=c("0", "3","1"),labels=c("Normal", "Empty","COVID")) + 
    xlab("Day") + ylab("Bed number") + 
    scale_y_continuous(lim=c(0,ymax)) +
    annotate(size = 2,'text',10, 15, 
             label=paste("Extra beds needed:",mm)) +
    geom_vline(xintercept = c(30,60,90),col="grey",lty = "dashed")
  
  ggsave(paste0("plots/extra_beds_",name,".pdf"))
  
}

plot_multiple <- function(M,name){
  
  miss <- M$miss
  miss_month <- M$miss_month
  
  p11 <- ggplot(miss, aes(x=day, y = m_norm)) + 
    geom_ribbon(aes(ymin = m_norm - sd_norm, ymax = m_norm + sd_norm), fill = "grey70") +
    geom_line(aes(y = m_norm)) + 
    annotate(size = 2,'text',(15 + 30*c(0,1,2)), -1, 
             label=paste("Av. miss. norm:",round(miss_month$m_norm,0))) + 
    annotate(size = 2,'text',(15 + 30*c(0,1,2)), -2, 
             label=paste("Av. miss. covid:",round(miss_month$m_cov,0))) +
    annotate(size = 2,'text',30, 12, 
             label=paste("Extra beds needed:",round(mean(M$max_bed_need),0),
                         "SD (",round(sd(M$max_bed_need),0),")")) + 
    ggtitle(name)
  
  ggsave(paste0("plots/miss_",name,".pdf"))
  
  stm <- cbind(seq(1,3,1),cbind(round(miss_month$m_norm,0),round(miss_month$m_cov,0)))
  write.csv(stm, paste0("outputs/",name,"_missedpermonth.csv"))
  
  write.csv(c(round(mean(M$max_bed_need),0),round(sd(M$max_bed_need),0)),paste0("outputs/",name,"_extrabed.csv"))
  write.csv(M$total_missed, paste0("outputs/",name,"_totalmissed.csv"))
}




