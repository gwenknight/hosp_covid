### functions_uclh_covid

##### Admission matrix

admin_ed_query_side_room <- function(cov_curve, norm_curve, paraq){ # Emergency department admissions to query COVID side rooms
  # cov_curve - incidence of COVID patients over time
  # norm_curve - incidence of normal ED patients over time
  # paraq - parameters for query rooms
  #qn = proportion of normal patients get tested for COVID
  #qc = proportion of COVID patients get tested for COVID
  
  ndays <- length(cov_curve) # how long for? 
  
  ## Side rooms
  SR <- as.data.frame(matrix(0,ndays,3))
  colnames(SR) <- c("day","norm_sr","cov_sr")
  SR[,"day"] <- seq(1,ndays,1)
  # Which normal patients go into side rooms? 
  SR[,"norm_sr"] <- rbinom(ndays, size = norm_curve, prob = para$qn)# Random sample for each patient
  SR[,"cov_sr"]  <- rbinom(ndays, size = cov_curve, prob = para$qc) # If below prob then sampled
  
  ## Admissions 
  A <- as.data.frame(matrix(0,ndays_i,4))
  colnames(A) <- c("day","norm_admin","cov_admin","prop_cov")
  A[,"day"] <- seq(1,ndays_i,1)
  A[,"norm_admin"] <- ceiling(norm_curve) #ceiling(rnorm(ndays_i,inc_rate,1)) #  depends on normal number of bed days / LOS of hospital
  A[,"cov_admin"] <- ceiling(cov_curve)
  A0 <- A
  
  ## Remove those that go to side rooms
  A$norm_admin <- A$norm_admin - SR$norm_sr
  A$cov_admin  <- A$cov_admin - SR$cov_sr
  A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin)
  
  ## Return SR needs and admin minus SR
  return(list(A = A, SR = SR))
  
}


### Bed filling function
# Takes number needed and fills beds

bed_filling_uclh <- function(admin, nbeds, los_norm, los_cov,para){
  # admin has number of COVID / NORMAL for admission
  # nbeds = beds to fill
  # los for both
  # para = probability discharge by COVID / NORMAL 
  
  ndays <- dim(admin)[1]
  
  #BEDS FILLED
  WC <- array(0,c(nbeds,2,(ndays_i+1))) # Array - 3D matrix. 
  colnames(WC)<-c("patno","status")
  #ADDITIONAL NEEDED
  WN <- array(0,c(1000,2,(ndays_i+1))) # Array - 3D matrix. 
  colnames(WN)<-c("patno","status")
  # rows = bed, columns = c(patient number, actual status, presumed status, days in hospital), 3D = time
  
  # Admission matrices
  A <- admin
  A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin) 
  
  # track number of patients 
  pat_num <- 0
  
  ###############**********************************************************************
  ### BEDS ###
  ###############**********************************************************************
  
  BEDS_filled <- as.data.frame(matrix(0,ndays_i,5)) # Want to know how many enter the ICU each day
  colnames(BEDS_fill) <- c("day","norm","cov","discharge","death") # FIXED COLUMN ORDER
  BEDS_fill$day <- seq(1,ndays_i,1)
  
  # fill by bed
  for(i in 1:nbeds){
    
    cumlos <- 0 # DAY ZERO
    
    # To fill beds from bottom up
    initial_fill <- ceiling(nbeds * (0.7 + runif(1)*0.2)) # Between 70-90% capacity
    if(i < initial_fill){
      # first patient in bed
      pat_num <- pat_num + 1
      pat_status <- 0 # normal patient in initially - assume v low prevalence of COVID
      los <- ceiling(rnorm(1,los_norm) * runif(1)) # been in for some time already perhaps
      BEDS_fill[1,(pat_status+2)] <- BEDS_fill[1,(pat_status+2)] + 1
    }else{
      pat_status <- 3 # Empty bed
      los <- 1 # check next day for patient
    }
    
    WC[i,c("patno","status"),1:los] <- c(pat_num,pat_status)
    ifelse(pat_status == 1, # if COVID
           pat_outcome <- ifelse(runif(1) < para$p_discharge_covid,0,1),
           pat_outcome <- ifelse(runif(1) < para$p_discharge_norm,0,1)) # 1 = death
    BEDS_fill[1,(pat_outcome+4)] <- BEDS_fill[1,(pat_outcome+4)] + 1
    
    cumlos <- cumlos + los
    
    while(cumlos < (ndays_i+1)){
      #print(cumlos)
      pat_num <- pat_num + 1 # Next patient
      ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
      if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
        #print(c(cumlos,pat_status))
        pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
        
        if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted
          
          BEDS_fill[cumlos,(pat_status+2)] <- BEDS_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          
          los <- ceiling(ifelse(pat_status == 1, 
                                sample(los_cov, 1, replace = TRUE), #rnorm(1,los_cov), 
                                rnorm(1,los_norm))) # length of stay
          
        }else{
          pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
          # don't need to check if patient of this type as know sum (A) > 0
          
          BEDS_fill[cumlos,(pat_status+2)] <- BEDS_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
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
        WC[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(0,pat_status) # this patient stays until end of los
      }else{
        WC[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(pat_num,pat_status) # this patient stays until end of los
      }
      
      cumlos <- cumlos + los # next new patient at this time point
      # Patient outcome
      ifelse(pat_status == 1, # if COVID
             pat_outcome <- ifelse(runif(1) < para$p_discharge_covid,0,1),
             pat_outcome <- ifelse(runif(1) < para$p_discharge_norm,0,1)) # 1 = death
      BEDS_fill[cumlos,(pat_outcome+4)] <- BEDS_fill[cumlos,(pat_outcome+4)] + 1
    }
    
  }
  Aleft <- A
  
  i = 1
  #print(pat_num)
  # Extra beds needed
  while( sum(A$norm_admin + A$cov_admin) > 0){
    
    cumlos <- 1
    WN[i,c("patno","status"),1] <- c(pat_num+1,3) # First day empty. A > 0 so will have a new patient to admit but not here and now
    
    while(cumlos < (ndays_i + 1)){
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
        WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(0,pat_status) # this patient stays until end of los
      }else{
        WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(pat_num,pat_status) # this patient stays until end of los
      }
      
      cumlos <- cumlos + los # next new patient at this time point
      # Patient outcome
      ifelse(pat_status == 1, # if COVID
             pat_outcome <- ifelse(runif(1) < para$p_discharge_covid,0,1),
             pat_outcome <- ifelse(runif(1) < para$p_discharge_norm,0,1)) # 1 = death
      BEDS_fill[cumlos,(pat_outcome+4)] <- BEDS_fill[cumlos,(pat_outcome+4)] + 1
      
      # print(c("4",cumlos, pat_num,pat_status))
    }
    i = i + 1 # move on to next bed
    
  }
  
  
  return(list(A0 = A0, A = A, Aleft = Aleft, WC = WC, WN = WN, pat_num = pat_num, BEDS_fill = BEDS_fill))
  
}








