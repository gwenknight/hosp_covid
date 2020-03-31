### PATIENT PATHWAY MODEL 

### Model setup
# (1) Take in COVID patient incidence prediction 
# (2) Determine which patients are level 1 (ward bed), level 2 (HDU), level 3 (ICU)
# (3) For each patient determine their pathway and length of stay in each 
# (4) Use above to determine admission matrices for each bed type (level 1-3)
# (5) Output # of beds needed, # and type of patient missed 


### PATHWAYS
n_pathways <- 3 # number of pathways
max_steps <- 3 # maximum number of movements

### LENGTH OF STAY IN EACH BED TYPE & PROBABILITY OF EACH
per_path <- matrix(0,1,n_pathways)
los_para_mn <- matrix(0,n_pathways,(max_steps))
los_para_sd <- matrix(0,n_pathways,(max_steps))

# pathway 1  ICU - HDU - WARD
per_path[1] <- 0.08 # proportion of incident cases that are bound for ICU on admission P1
los_para_mm[1,] <- c(14,7,14)
los_para_sd[1,] <- c(1,1,1) # NO DATA ON SD ATM

# pathway 2 HDU - WARD
per_path[2] <- 0.06 # proportion of incident cases that are bound for HDU on admission P2
los_para_mm[2,] <- c(7,10,0)
los_para_sd[2,] <- c(1,1,0) # NO DATA ON SD ATM

# pathway 3
per_path[3] <- 1 - sum(per_path[1:2]) # OTHER covid patients 
los_para_mm[3,] <- c(10,0,0) # NOT BEEN GIVEN THIS YET - ASSUMING LOWEST IN LINE WITH P2
los_para_sd[3,] <- c(1,0,0)


### INPUTS - may have to write this to loop over several scenarios. 
# Could read in big matrix with each column a scenario
cov_curve <- read.csv("UCLH/data/200326_pietro_preds.csv")[1:60,] # date / COVID patient incidence 

# FOR EACH SCENARIO
total_patients <- sum(cov_curve$covid_curve)

# EACH DAY APPLY THE PROPORTIONS
# THEN ASSIGN LOS

# FOR EACH DAY
for(i in 1:length(cov_curve[,1])){
  
  ##********** ADMISSIONS **********##
  npat <- cov_curve[i,2] # number of patients on that day
  
  ## PATHWAY TYPE  
  s <- sample(seq(1,n_pathways,1), npat, prob = per_path, replace = TRUE) 
  
  ##**********  FILL ICU **********##
  # Admissions 
  ##**********  THEN FILL HDU **********##
  # Admissions + ICU left to HDU
  ##**********  THEN FILL WARDS **********##
  # Admissions + HDU left 
  
  
  
}



###### BED FILLING ALGORITHM
## TAKE IN INCIDENCE ACROSS PATIENT TYPES - instead of normal vs covid do admission vs coming from ICU coming from HDU
## 
## INPUTS
# (1) number of beds (May need to have nbeds over time) 
# (2) los per bed type 
# (3) 

## Admissions 
A <- as.data.frame(matrix(0,ndays_i,4))
colnames(A) <- c("day","norm_admin","cov_admin","prop_cov")
A[,"day"] <- seq(1,ndays_i,1)
A[,"norm_admin"] <- ceiling(norm_curve) #ceiling(rnorm(ndays_i,inc_rate,1)) #  depends on normal number of bed days / LOS of hospital
A[,"cov_admin"] <- ceiling(cov_curve)
A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin)
A0 <- A


bed_filling_pathway <- function(nbeds, A
                                los_norm, los_cov, 
                                cov_curve, norm_curve, ndays_i = 50, pdischarge){
  
  
  
  #CRITICAL BED DAYS
  WC <- array(0,c(nbeds,2,(ndays_i+1))) # Array - 3D matrix. 
  colnames(WC)<-c("patno","status")
  #NEW BEDS NEEDED
  WN <- array(0,c(3000,2,(ndays_i+1))) # Array - 3D matrix. 
  colnames(WN)<-c("patno","status")
  # rows = bed, columns = c(patient number, actual status, presumed status, days in hospital), 3D = time
  
  # track number of patients 
  pat_num <- 0
  
  ###############**********************************************************************
  ### ICU BEDS ###
  ###############**********************************************************************
  
  # Want to know how many in the ICU per day
  # And how many discharge / deaths
  ICU_fill <- as.data.frame(matrix(0,ndays_i,5)) # 9 AGE groups
  colnames(ICU_fill) <- c("day","norm","cov","discharge","death") # FIXED COLUMN ORDER
  ICU_fill$day <- seq(1,ndays_i,1)
  
  # fill by bed
  for(i in 1:nbeds){
    
    cumlos <- 0 # DAY ZERO
    
    # To fill beds from bottom up
    initial_fill <- ceiling(nbeds * (0.7 + runif(1)*0.2)) # Between 70-90% capacity
    if(i < initial_fill){
      # first patient in bed
      pat_num <- pat_num + 1
      pat_status <- 0 # normal patient in initially - assume v low prevalence of COVID
      if(length(los_norm)==2){
        los <- ceiling(rnorm(1,los_norm) * runif(1)) # been in for some time already perhaps
      }else{
        los <- ceiling(runif(1) * rgamma(1,shape = los_norm["shape"], rate = los_norm["rate"]))# GAMMA 
      }
      ICU_fill[1,(pat_status+2)] <- ICU_fill[1,(pat_status+2)] + 1
    }else{
      pat_status <- 3 # Empty bed
      los <- 1 # check next day for patient
    }
    
    WC[i,c("patno","status"),pmin(1:los, ndays_i)] <- c(pat_num,pat_status)
    
    cumlos <- cumlos + los
    # Patient outcome
    if(cumlos <= ndays_i){
      pat_outcome <- ifelse(runif(1) < ifelse(pat_status == 0, pdischarge$covid, pdischarge$normal), 0, 1) # 1 = death 
      ICU_fill[cumlos,(pat_outcome+4)] <- ICU_fill[cumlos,(pat_outcome+4)] + 1
    }
    
    # For this bed, whilst we still have days left to fill
    while(cumlos < (ndays_i+1)){
      
      pat_num <- pat_num + 1 # Next patient
      
      ### NEED TO HAVE PATIENTS TO ADMIT
      if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
        
        # First come first served - so randomly pick. BUT if none of this type then switch status (below)
        pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
        
        # If normal - count them as appearing / remove from admin / regen proportion covid / generate their los
        if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted
          
          ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0) # update proportion covid
          
          # Different possible inputs for los: normal distibution if 2 para, gamma if 3
          if(length(los_norm)==2){
            los <- ceiling(ifelse(pat_status == 1,sample(los_cov, 1, replace = TRUE),rnorm(1,los_norm))) # NORM  
          }else{
            los <- ceiling(ifelse(pat_status == 1, sample(los_cov, 1, replace = TRUE),rgamma(1,shape = los_norm["shape"], rate = los_norm["rate"]))) # GAMMA
          }
          
          
        }else{ # If none of first picked status then
          pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
          # don't need to check if patient of this type as know sum (A) > 0
          
          ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          
          # LOS for this patient
          if(length(los_norm)==2){
            los <- ceiling(ifelse(pat_status == 1, sample(los_cov, 1, replace = TRUE),rnorm(1,los_norm))) # length of stay  
          }else{
            los <- ceiling(ifelse(pat_status == 1, sample(los_cov, 1, replace = TRUE), rgamma(1,shape = los_norm["shape"], rate = los_norm["rate"]))) # GAMMA
          }
          
        }
      }else{ # If not patient then skip this day 
        los <- 1
        pat_status <- 3 # EMPTY BED
        pat_num <- pat_num - 1 # No patient
      }
      
      # If empty bed don't store patient number 
      if(pat_status == 3){
        WC[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(0,pat_status) # this patient stays until end of los
      }else{ # Store patient
        WC[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(pat_num,pat_status) # this patient stays until end of los
      }
      
      cumlos <- cumlos + los # next new patient at this time point
      # AND patient outcome
      if(pat_status != 3){
        if(cumlos <= ndays_i){
          pat_outcome <- ifelse(runif(1) < ifelse(pat_status == 0, pdischarge$covid, pdischarge$normal), 0, 1) # 1 = death 
          ICU_fill[cumlos,(pat_outcome+4)] <- ICU_fill[cumlos,(pat_outcome+4)] + 1
        }
      }
      
    }
    
  }
  Aleft <- A
  i = 1
  
  # Extra beds needed
  while( sum(A$norm_admin + A$cov_admin) > 0){
    
    cumlos <- 1
    WN[i,c("patno","status"),1] <- c(0,3) # First day empty. A > 0 so will have a new patient to admit but not here and now
    
    while(cumlos < (ndays_i + 1)){
      
      pat_num <- pat_num + 1 # Next patient
      
      ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
      if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
        
        pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
        
        if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted on that day
          
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          
          # LOS for this patient
          if(length(los_norm)==2){
            los <- ceiling(ifelse(pat_status == 1, sample(los_cov, 1, replace = TRUE),rnorm(1,los_norm))) # length of stay  
          }else{
            los <- ceiling(ifelse(pat_status == 1, sample(los_cov, 1, replace = TRUE), rgamma(1,shape = los_norm["shape"], rate = los_norm["rate"]))) # GAMMA
          }
          
        }else{
          pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
          # don't need to check if patient of this type as know sum (A) > 0
          
          A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
          A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,
                                         A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
          
          # LOS for this patient
          if(length(los_norm)==2){
            los <- ceiling(ifelse(pat_status == 1, sample(los_cov, 1, replace = TRUE),rnorm(1,los_norm))) # length of stay  
          }else{
            los <- ceiling(ifelse(pat_status == 1, sample(los_cov, 1, replace = TRUE), rgamma(1,shape = los_norm["shape"], rate = los_norm["rate"]))) # GAMMA
          }
        }
      }else{
        los <- 1
        pat_status <- 3 # EMPTY BED
        pat_num <- pat_num - 1 # No patient
      }
      
      # If empty bed don't store patient number 
      if(pat_status == 3){
        WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(0,pat_status) # this patient stays until end of los
      }else{
        WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(pat_num,pat_status) # this patient stays until end of los
      }
      
      cumlos <- cumlos + los # next new patient at this time point
      # AND patient outcome
      if(pat_status != 3){
        if(cumlos <= ndays_i){
          pat_outcome <- ifelse(runif(1) < ifelse(pat_status == 0, pdischarge$covid, pdischarge$normal), 0, 1) # 1 = death 
          ICU_fill[cumlos,(pat_outcome+4)] <- ICU_fill[cumlos,(pat_outcome+4)] + 1
        }
      }
    }
    i = i + 1 # move on to next bed
    
  }
  
  return(list(A0 = A0, A = A, Aleft = Aleft, WC = WC, WN = WN, pat_num = pat_num, ICU_fill = ICU_fill))
  
}




