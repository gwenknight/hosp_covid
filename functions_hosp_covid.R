## FUNCTIONS
### Fills beds for 6 months. Daily time step
# nbeds = number of beds in ward
# los = length of stay (mean, sd) for normal / covid patients
# cov_curve = changing NUMBER of patients that are covid+
# inc_rate = ICNHT data

bed_filling <- function(nbeds, los_norm, los_cov, cov_curve, norm_curve, ndays_i = 50, pdischarge){
  
  ## Admissions 
  A <- as.data.frame(matrix(0,ndays_i,4))
  colnames(A) <- c("day","norm_admin","cov_admin","prop_cov")
  A[,"day"] <- seq(1,ndays_i,1)
  A[,"norm_admin"] <- ceiling(norm_curve) #ceiling(rnorm(ndays_i,inc_rate,1)) #  depends on normal number of bed days / LOS of hospital
  A[,"cov_admin"] <- ceiling(cov_curve)
  A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin)
  A0 <- A
  
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

# #bed_filling_uclh <- function(nbeds, los_norm, los_cov, cov_curve, norm_curve, ndays_i = 50){
# 
# ## Admissions 
# A <- as.data.frame(matrix(0,ndays_i,4))
# colnames(A) <- c("day","norm_admin","cov_admin","prop_cov")
# A[,"day"] <- seq(1,ndays_i,1)
# A[,"norm_admin"] <- ceiling(norm_curve) #ceiling(rnorm(ndays_i,inc_rate,1)) #  depends on normal number of bed days / LOS of hospital
# A[,"cov_admin"] <- ceiling(cov_curve)
# A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin)
# A0 <- A
# 
# #CRITICAL BED DAYS
# WC <- array(0,c(nbeds,2,(ndays_i+1))) # Array - 3D matrix. 
# colnames(WC)<-c("patno","status")
# #NEW BEDS NEEDED
# WN <- array(0,c(1000,2,(ndays_i+1))) # Array - 3D matrix. 
# colnames(WN)<-c("patno","status")
# # rows = bed, columns = c(patient number, actual status, presumed status, days in hospital), 3D = time
# 
# # track number of patients 
# pat_num <- 0
# 
# ###############**********************************************************************
# ### ICU BEDS ###
# ###############**********************************************************************
# 
# ICU_fill <- as.data.frame(matrix(0,ndays_i,3)) # Want to know how many enter the ICU each day
# colnames(ICU_fill) <- c("day","norm","cov")
# ICU_fill$day <- seq(1,ndays_i,1)
# 
# # fill by bed
# for(i in 1:nbeds){
#   
#   cumlos <- 0 # DAY ZERO
#   
#   # To fill beds from bottom up
#   initial_fill <- ceiling(nbeds * (0.7 + runif(1)*0.2)) # Between 70-90% capacity
#   if(i < initial_fill){
#     # first patient in bed
#     pat_num <- pat_num + 1
#     pat_status <- 0 # normal patient in initially - assume v low prevalence of COVID
#     los <- ceiling(rnorm(1,los_norm) * runif(1)) # been in for some time already perhaps
#     ICU_fill[1,(pat_status+2)] <- ICU_fill[1,(pat_status+2)] + 1
#   }else{
#     pat_status <- 3 # Empty bed
#     los <- 1 # check next day for patient
#   }
#   
#   WC[i,c("patno","status"),1:los] <- c(pat_num,pat_status)
#   
#   cumlos <- cumlos + los
#   
#   while(cumlos < (ndays_i+1)){
#     #print(cumlos)
#     pat_num <- pat_num + 1 # Next patient
#     ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
#     if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
#       #print(c(cumlos,pat_status))
#       pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
#       
#       if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted
#         
#         ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
#         A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
#         A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
#         
#         los <- ceiling(ifelse(pat_status == 1, 
#                               sample(los_cov, 1, replace = TRUE), #rnorm(1,los_cov), 
#                               rnorm(1,los_norm))) # length of stay
#         
#       }else{
#         pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
#         # don't need to check if patient of this type as know sum (A) > 0
#         
#         ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
#         A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
#         A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,
#                                        A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
#         
#         
#         los <- ceiling(ifelse(pat_status == 1, 
#                               sample(los_cov, 10, replace = TRUE),#rnorm(1,los_cov), 
#                               rnorm(1,los_norm))) # length of stay
#         
#       }
#     }else{
#       los <- 1
#       pat_status <- 3 # EMPTY BED
#       pat_num <- pat_num - 1 # No patient
#     }
#     
#     # If empty bed don't store patient number 
#     if(pat_status == 3){
#       WC[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(0,pat_status) # this patient stays until end of los
#     }else{
#       WC[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(pat_num,pat_status) # this patient stays until end of los
#     }
#     
#     cumlos <- cumlos + los # next new patient at this time point
#   }
#   
# }
# Aleft <- A
# 
# i = 1
# #print(pat_num)
# # Extra beds needed
# while( sum(A$norm_admin + A$cov_admin) > 0){
#   
#   cumlos <- 1
#   WN[i,c("patno","status"),1] <- c(pat_num+1,3) # First day empty. A > 0 so will have a new patient to admit but not here and now
#   
#   while(cumlos < (ndays_i + 1)){
#     #print(c("1",cumlos, pat_num,pat_status))
#     pat_num <- pat_num + 1 # Next patient
#     #  print(c("2",cumlos, pat_num,pat_status))
#     ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
#     if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
#       #print(c(cumlos,pat_status))
#       
#       pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
#       
#       if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted on that day
#         
#         A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
#         A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
#         los <- ceiling(ifelse(pat_status == 1, 
#                               sample(los_cov, 1, replace = TRUE), #rnorm(1,los_cov), 
#                               rnorm(1,los_norm))) # length of stay
#         
#       }else{
#         pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
#         # don't need to check if patient of this type as know sum (A) > 0
#         
#         A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
#         A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,
#                                        A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
#         los <- ceiling(ifelse(pat_status == 1, 
#                               sample(los_cov, 10, replace = TRUE),#rnorm(1,los_cov), 
#                               rnorm(1,los_norm))) # length of stay
#       }
#     }else{
#       los <- 1
#       pat_status <- 3 # EMPTY BED
#       pat_num <- pat_num - 1 # No patient
#     }
#     # print(c("3",cumlos, pat_num,pat_status))
#     # If empty bed don't store patient number 
#     if(pat_status == 3){
#       WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(0,pat_status) # this patient stays until end of los
#     }else{
#       WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(pat_num,pat_status) # this patient stays until end of los
#     }
#     
#     cumlos <- cumlos + los # next new patient at this time point
#     # print(c("4",cumlos, pat_num,pat_status))
#   }
#   i = i + 1 # move on to next bed
#   
# }
# 
# 
# return(list(A0 = A0, A = A, Aleft = Aleft, WC = WC, WN = WN, pat_num = pat_num, ICU_fill = ICU_fill))
# 
# }

# bed_filling_age <- function(nbeds, los_norm, los_cov, cov_curve, norm_curve, ndays_i = 90, age_data){
#   
#   ## Admissions 
#   An <- as.data.frame(matrix(0,ndays_i,4)) # NORMAL
#   Ac <- as.data.frame(matrix(0,ndays_i,4)) # COVID
#   colnames(An) <- c("day",seq(1,9,1)) # day then ages
#   colnames(An) <- c("day",seq(1,9,1)) # day then ages
#   
#   
#   A[,"day"] <- seq(1,ndays_i,1)
#   A[,"norm_admin"] <- ceiling(norm_curve) #ceiling(rnorm(ndays_i,inc_rate,1)) #  depends on normal number of bed days / LOS of hospital
#   A[,"cov_admin"] <- ceiling(cov_curve)
#   A[,"prop_cov"] <- A$cov_admin/(A$norm_admin + A$cov_admin)
#   A0 <- A
#   
#   #CRITICAL BED DAYS
#   WC <- array(0,c(nbeds,3,(ndays_i+1))) # Array - 3D matrix. 
#   colnames(WC)<-c("patno","status","pat_age")
#   #NEW BEDS NEEDED
#   WN <- array(0,c(5000,3,(ndays_i+1))) # Array - 3D matrix. 
#   colnames(WN)<-c("patno","status","pat_age")
#   # rows = bed, columns = c(patient number, actual status, presumed status, days in hospital), 3D = time
#   
#   # track number of patients 
#   pat_num <- 0
#   
#   ###############**********************************************************************
#   ### ICU BEDS ###
#   ###############**********************************************************************
#   
#   # Want to know how many enter the ICU each day
#   # And how many discharge / deaths
#   ICU_fill <- as.data.frame(matrix(0,9*ndays_i,5)) # 9 AGE groups
#   colnames(ICU_fill) <- c("day","norm","cov","discharge","death") # FIXED COLUMN ORDER
#   ICU_fill$day <- seq(1,ndays_i,1)
#   
#   # fill by bed
#   for(i in 1:nbeds){
#     
#     cumlos <- 0 # DAY ZERO
#     
#     # first patient in bed
#     pat_num <- pat_num + 1
#     pat_age <- sample(seq(1,9,1),prob=age_data$prop,1) # sample from region population atm - could be differs for ICU
#     if(runif(1) < 0.2){ # 20% empty beds
#       pat_status <- 3 # Empty bed
#       los <- 1 # check next day for patient
#     }else{
#       pat_status <- 0 # normal patient in initially - assume v low prevalence of COVID
#       los <- ceiling(rnorm(1,los_norm) * runif(1)) # been in for some time already perhaps
#       ICU_fill[1,(pat_status+2)] <- ICU_fill[1,(pat_status+2)] + 1
#     }
#     
#     WC[i,c("patno","status","pat_age"),1:los] <- c(pat_num,pat_status,pat_age)
#     pat_outcome <- ifelse(runif(1) < age_data[pat_age,"p_discharge"],0,1) # 1 = death
#     ICU_fill[1,(pat_outcome+4)] <- ICU_fill[1,(pat_outcome+4)] + 1
#     
#     cumlos <- cumlos + los
#     
#     # FILL BEDS
#     while(cumlos < (ndays_i+1)){
#       #print(cumlos)
#       pat_num <- pat_num + 1 # Next patient
#       ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
#       if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
#         #print(c(cumlos,pat_status))
#         pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
#         if(pat_status == 1){pat_age <- sample(seq(1,9,1),prob = A$cov_admin[cumlos],1)} # Sample age from covid patients
#         if(pat_status == 0){pat_age <- sample(seq(1,9,1),prob = A$norm_admin[cumlos],1)} # Sample age from norm patients
#         
#         if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted
#           
#           ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
#           A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
#           A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
#           
#           los <- ceiling(ifelse(pat_status == 1, 
#                                 sample(los_cov, 1), #rnorm(1,los_cov), 
#                                 rnorm(1,los_norm))) # length of stay
#           
#           
#           
#         }else{
#           pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
#           # don't need to check if patient of this type as know sum (A) > 0
#           
#           ICU_fill[cumlos,(pat_status+2)] <- ICU_fill[cumlos,(pat_status+2)] + 1 # count of filled beds
#           A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
#           A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,
#                                          A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
#           
#           
#           los <- ceiling(ifelse(pat_status == 1, 
#                                 sample(los_cov, 10, replace = TRUE),#rnorm(1,los_cov), 
#                                 rnorm(1,los_norm))) # length of stay
#           
#         }
#       }else{
#         los <- 1
#         pat_status <- 3 # EMPTY BED
#         pat_num <- pat_num - 1 # No patient
#       }
#       
#       # If empty bed don't store patient number 
#       if(pat_status == 3){
#         WC[i,c("patno","status","pat_age"),pmin((cumlos + (1:los)),ndays_i)] <- c(0,pat_status,0) # this patient stays until end of los
#       }else{
#         WC[i,c("patno","status","pat_age"),pmin((cumlos + (1:los)),ndays_i)] <- c(pat_num,pat_status,pat_age) # this patient stays until end of los
#       }
#       
#       cumlos <- cumlos + los # next new patient at this time point
#       # Patient outcome
#       pat_outcome <- ifelse(runif(1) < age_data[pat_age,"p_discharge"],0,1) # 1 = death
#       ICU_fill[cumlos,(pat_outcome+4)] <- ICU_fill[cumlos,(pat_outcome+4)] + 1
#     }
#     
#   }
#   Aleft <- A
#   
#   i = 1
#   #print(pat_num)
#   # Extra beds needed
#   while( sum(A$norm_admin + A$cov_admin) > 0){
#     
#     cumlos <- 1
#     WN[i,c("patno","status"),1] <- c(pat_num+1,3) # First day empty. A > 0 so will have a new patient to admit but not here and now
#     
#     while(cumlos < (ndays_i + 1)){
#       #print(c("1",cumlos, pat_num,pat_status))
#       pat_num <- pat_num + 1 # Next patient
#       
#       #  print(c("2",cumlos, pat_num,pat_status))
#       ### COMPLICATED BY NEED TO HAVE PATIENTS TO ADMIT
#       if(sum(A[cumlos,c("norm_admin","cov_admin")]) > 0){ # if there is a patient to be admitted
#         #print(c(cumlos,pat_status))
#         
#         pat_status <- ifelse(runif(1) < A$prop_cov[cumlos],1,0) # 1 = COVID positive or not (0)?
#         if(pat_status == 1){pat_age <- sample(seq(1,9,1),prob = A$cov_admin[cumlos],1)} # Sample age from covid patients
#         if(pat_status == 0){pat_age <- sample(seq(1,9,1),prob = A$norm_admin[cumlos],1)} # Sample age from norm patients
#         
#         if(A[cumlos,(pat_status+2)] > 0){ # if there is a patient of this type to be admitted on that day
#           
#           A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
#           A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
#           los <- ceiling(ifelse(pat_status == 1, 
#                                 sample(los_cov, 1, replace = TRUE), #rnorm(1,los_cov), 
#                                 rnorm(1,los_norm))) # length of stay
#           
#         }else{
#           pat_status <- ifelse(pat_status == 1, 0, 1) # switch status
#           # don't need to check if patient of this type as know sum (A) > 0
#           
#           A[cumlos, (pat_status+2)] <- A[cumlos, (pat_status+2)] - 1 # remove from admin
#           A[cumlos,"prop_cov"] <- ifelse((A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"])>0,
#                                          A[cumlos,"cov_admin"] / (A[cumlos,"norm_admin"] + A[cumlos,"cov_admin"]),0)
#           los <- ceiling(ifelse(pat_status == 1, 
#                                 sample(los_cov, 10, replace = TRUE),#rnorm(1,los_cov), 
#                                 rnorm(1,los_norm))) # length of stay
#         }
#       }else{
#         los <- 1
#         pat_status <- 3 # EMPTY BED
#         pat_num <- pat_num - 1 # No patient
#       }
#       # print(c("3",cumlos, pat_num,pat_status))
#       # If empty bed don't store patient number 
#       
#       if(pat_status == 3){
#         WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(0,pat_status) # this patient stays until end of los
#       }else{
#         WN[i,c("patno","status"),pmin((cumlos + (1:los)),ndays_i)] <- c(pat_num,pat_status) # this patient stays until end of los
#       }
#       
#       cumlos <- cumlos + los # next new patient at this time point
#       # Patient outcome
#       pat_outcome <- ifelse(runif(1) < age_data[pat_age,"p_discharge"],0,1) # 1 = death
#       ICU_fill[cumlos,(pat_outcome+4)] <- ICU_fill[cumlos,(pat_outcome+4)] + 1
#       
#       # print(c("4",cumlos, pat_num,pat_status))
#     }
#     i = i + 1 # move on to next bed
#     
#   }
#   
#   
#   return(list(A0 = A0, A = A, Aleft = Aleft, WC = WC, WN = WN, pat_num = pat_num, ICU_fill = ICU_fill))
#   
# }

### Multiple runs

multiple_runs <- function(nruns, nbeds, los_norm, los_cov, cov_curve, inc_rate, ndays_i = 50,pdischarge){
  
  h_store<-c()
  missing_store <- c() #matrix(0,100*ndays_i,5)
  bed_store <- c()
  max_bed_need <- c()
  miss_norm <- c()
  miss_covi <- c()
  outcomes_store <- c()
  
  for(j in 1:nruns){
    if(length(inc_rate == 1)){norm_curve <- rnorm(ndays_i,inc_rate,1)
    }else{norm_curve <- inc_rate}
    
    output <- bed_filling(nbeds, los_norm, los_cov, cov_curve, norm_curve, ndays_i = 50,pdischarge)
    
    h <- melt(output$WC,id.vars = "patno")
    colnames(h) <- c("bedno","variable","time","value")
    h <- dcast(h, time + bedno ~variable)
    h <- h[-which(h$time == (ndays_i + 1)),]
    
    h_store <- rbind(h_store, cbind(j,h))
    missing_store <- rbind(missing_store, cbind(j,output$Aleft))
    
    # Extra beds need 
    n <- melt(output$WN,id.vars = "patno")
    colnames(n) <- c("bedno","variable","time","value")
    n <- dcast(n, time + bedno ~variable)
    n <- n[-which(n$time == (ndays_i+1)),]
    
    ll <- n %>% group_by(bedno) %>% summarise(mean(patno)) # which beds actually have patients in
    mm <- ifelse(length(which(ll[,2]>0,arr.ind = TRUE))>0,max(which(ll[,2]>0,arr.ind = TRUE)),0) # max bed number
    ww <- which(n$bedno <= mm)
    n <- n[ww,]
    
    # Some of them have enough beds
    if(dim(n)[1] == 0){n <- c(0,0,0,3); bed_store <- rbind(bed_store, c(j,n))
    }else{bed_store <- rbind(bed_store, cbind(j,n))}
    
    
    max_bed_need <- c(max_bed_need,mm) # This value is the number of beds
    miss_norm <- c(miss_norm, sum(output$Aleft[,c("norm_admin")])) # This value is the number of patients
    miss_covi <- c(miss_covi, sum(output$Aleft[,c("cov_admin")])) 
    
    outcomes_store <- rbind(outcomes_store, cbind(j,output$ICU_fill[,c("day","death","discharge")]))
  }
  
  ## Multiple runs
  h_store <- as.data.frame(h_store)
  missing_store <- as.data.frame(missing_store)
  miss <- missing_store %>% group_by(day) %>% 
    summarise(m_norm = mean(norm_admin), m_cov = mean(cov_admin), 
              sd_norm = sd(norm_admin), sd_cov = sd(cov_admin)) 
  
  # mean number missed per day
  #missing_store$month <- rep(1:(ndays_i/30),each = 30)
  #miss_month <- missing_store %>% group_by(month) %>% 
  #summarise(m_norm = mean(norm_admin), m_cov = mean(cov_admin), 
  #           sd_norm = sd(norm_admin), sd_cov = sd(cov_admin)) 
  
  # mean total missed
  total_missed <- c(mean(miss_norm), sd(miss_norm), 
                    mean(miss_covi), sd(miss_covi))
  
  bed_store <- as.data.frame(bed_store)
  colnames(bed_store) <- c("j","time","bedno","patno","status")
  ### ADD TOGETHER TO GIVE RUNNING TOTAL
  bed_store$bedno <- bed_store$bedno + nbeds
  total_bed_all <- rbind(h_store, bed_store)
  w <- which(total_bed_all$status != 3)
  total_beds <- as.data.frame(total_bed_all[w,])
  total_beds_mn <- total_beds %>% group_by(j, bedno) %>% slice(which.min(time)) # time need bed
  total_beds_time <- total_beds_mn %>% group_by(time) %>% summarize(mbed = mean(bedno,na.rm = TRUE), sdbed = sd(bedno,na.rm = TRUE))
  
  ## outcomes
  outcomes_store<-as.data.frame(outcomes_store)
  colnames(outcomes_store) <- c("run","day","death","discharge")
  outcomes_smr <- outcomes_store %>% group_by(day) %>% 
    summarize(mdeaths = mean(death,na.rm = TRUE), sddeath = sd(death,na.rm = TRUE),
              mdischarge = mean(discharge,na.rm = TRUE), sddischarge = sd(discharge,na.rm = TRUE))
  
  
  return(list(missing_store = missing_store,# miss_month = miss_month,
              h_store = h_store, miss = miss,
              bed_store = bed_store, max_bed_need = max_bed_need,
              total_missed = total_missed, 
              total_beds_time = total_beds_time, outcomes_smr = outcomes_smr))
  
}

sigmoid = function(params, x) {
  params[1] / (1 + exp(-params[2] * (x - params[3])))
}


#### E.G. PLOT

## e.g.

plot_eg <- function(output, name, norm_curve, cov_curve){
  
  ndays_i <- length(norm_curve)
  cols <- c("3" = "lightblue", "1" = "orange", "0" = "purple")
  # Grab data
  # Basic time data to ndays_i days
  h <- melt(output$WC,id.vars = "patno")
  colnames(h) <- c("bedno","variable","time","value")
  h <- dcast(h, time + bedno ~variable)
  h <- h[-which(h$time == (ndays_i+1)),]
  
  # Plot this
  p1 <- ggplot(h, aes(x = time, y = bedno) ) + 
    geom_point(aes(col = factor(status))) + 
    scale_colour_manual(name  ="Status",values = cols,breaks=c("0", "3","1"),labels=c("Normal", "Empty","COVID")) + 
    xlab("Day") + ylab("Bed number") #+
  #geom_vline(xintercept = c(30,60,90),col="grey",lty = "dashed")
  
  pcov <- as.data.frame(cbind(seq(1,length(cov_curve),1),cov_curve));
  colnames(pcov)<-c("days","cprev")
  pcov$norm <- norm_curve
  pcovm <- melt(pcov, id.vars = "days")
  
  p2 <- ggplot(pcovm, aes(x=days, y = value)) + geom_line(aes(group = variable, col = variable)) + 
    scale_x_continuous("Day") + scale_y_continuous("Daily admission\nneed") +
    #geom_vline(xintercept = c(30,60,90),col="grey",lty = "dashed") + 
    scale_colour_manual(name ="Status",values=c("norm"="purple","cprev" = "orange"),labels=c("COVID","Normal")) +
    geom_hline(yintercept = mean(norm_curve), lty = "dotted") + 
    annotate(size = 2,'text',10, round(mean(norm_curve)+2), 
             label=paste("Mean norm. (daily):",round(mean(norm_curve))))
  
  # Missing people
  miss <- melt(output$Aleft[,c("day","norm_admin","cov_admin")], id.vars = "day")
  #perc_not_treat <- round(100*output$pat_num/(output_nocovid$pat_num),0)
  
  p3 <- ggplot(miss, aes(fill=variable, y=value, x=day)) + 
    geom_bar(position="stack", stat="identity") + 
    scale_fill_manual(name  ="Status",values=c("norm_admin"="purple","cov_admin" = "orange"),labels=c("Normal", "COVID")) +
    scale_y_continuous("Extra bed space\nneeded per day") + 
    scale_x_continuous("Day") + 
    #annotate(size = 2,'text',90, 20, 
    # label=paste("Percentage of normal ICU burden treated:",perc_not_treat,"%"))+ 
    annotate(size = 2,'text',10, 19, 
             label=paste("Total missed norm:",sum(output$Aleft[,c("norm_admin")]))) +
    annotate(size = 2,'text',10, 15, 
             label=paste("Total missed covid:",sum(output$Aleft[,c("cov_admin")])))# +
  #geom_vline(xintercept = c(30,60,90),col="grey",lty = "dashed")
  
  
  p2/p1+p3
  ggsave(paste0("plots/eg_",name,".pdf"))
  
  ### Beds needed
  n <- melt(output$WN,id.vars = "patno")
  colnames(n) <- c("bedno","variable","time","value")
  n <- dcast(n, time + bedno ~variable)
  n <- n[-which(n$time == (ndays_i+1)),]
  
  ll <- n %>% group_by(bedno) %>% summarise(mean(patno)) # which beds actually have patients in
  mm <- max(which(ll[,2]>0,arr.ind = TRUE)) # max bed number
  ww <- which(n$bedno <= mm)
  n <- n[ww,]
  
  ymax = ifelse(mm > 500,1000,50)
  
  g <- ggplot(n, aes(x = time, y = bedno) ) + 
    geom_point(aes(col = factor(status))) + 
    scale_colour_manual(name  ="Status",values = cols,breaks=c("0", "3","1"),labels=c("Normal", "Empty","COVID")) + 
    xlab("Day") + ylab("Bed number") + 
    scale_y_continuous(lim=c(0,ymax)) 
  annotate(size = 2,'text',10, 15, 
           label=paste("Extra beds needed:",mm)) #+
  #geom_vline(xintercept = c(30,60,90),col="grey",lty = "dashed")
  
  ggsave(paste0("plots/extra_beds_",name,".pdf"))
  
  (p2+p1)/(p3+g)
  ggsave(paste0("plots/eg_square_",name,".pdf"))
  
  ### DEATHS/DISCHARGE
  outcomes <- melt(output$ICU_fill[,c("day","death","discharge")], id.vars = "day")
  g2 <- ggplot(outcomes, aes(x=day,y=value)) + geom_line(aes(group = variable, colour = variable)) +
    scale_x_continuous("Day") + 
    scale_y_continuous("Number") 
  ggsave(paste0("plots/eg_outcomes_",name,".pdf"))
}

plot_multiple <- function(M,name){
  
  miss <- M$miss
  miss_month <- M$miss_month
  nbeds <- max(M$h_store$bedno)
  
  p11 <- ggplot(miss, aes(x=day, y = m_norm)) + 
    geom_ribbon(aes(ymin = m_norm - sd_norm, ymax = m_norm + sd_norm), fill = "grey70") +
    geom_line(aes(y = m_norm)) + ggtitle(name)
  #annotate(size = 2,'text',(15 + 30*c(0,1,2)), -1, 
  #         label=paste("Av. miss. norm:",round(miss_month$m_norm,0))) + 
  #annotate(size = 2,'text',(15 + 30*c(0,1,2)), -2, 
  #         label=paste("Av. miss. covid:",round(miss_month$m_cov,0))) +
  #annotate(size = 2,'text',30, 12, 
  #         label=paste("Extra beds needed:",round(mean(M$max_bed_need),0),
  #                     "SD (",round(sd(M$max_bed_need),0),")")) + 
  #
  
  ggsave(paste0("plots/miss_",name,".pdf"))
  
  # stm <- cbind(seq(1,3,1),cbind(round(miss_month$m_norm,0),round(miss_month$m_cov,0)))
  #write.csv(stm, paste0("outputs/",name,"_missedpermonth.csv"))
  
  write.csv(c(round(mean(M$max_bed_need),0),round(sd(M$max_bed_need),0)),paste0("outputs/",name,"_extrabed.csv"))
  write.csv(M$total_missed, paste0("outputs/",name,"_totalmissed.csv"))
  
  p22 <- ggplot(M$total_beds_time,aes(x=time, y = mbed)) + 
    geom_ribbon(aes(ymin = mbed - sdbed, ymax = mbed +sdbed), fill="grey70") + 
    geom_line(aes(y = mbed)) + 
    scale_y_continuous("Number of beds needed") + 
    scale_x_continuous("Days") + 
    ggtitle(name) + 
    geom_hline(yintercept = nbeds, col = "red")
  
  ggsave(paste0("plots/bed_need_overtime_",name,".pdf"))
  
  ### DEATHS/DISCHARGE
  outcomes <- M$outcomes_smr
  p33 <- ggplot(outcomes, aes(x=day,y=mdeaths)) + geom_ribbon(aes(ymin = mdeaths - sddeath, ymax = mdeaths + sddeath), fill = "red", alpha = 0.5) + 
    geom_line(aes(y=mdeaths)) + 
    geom_ribbon(aes(ymin=mdischarge-sddischarge, ymax = mdischarge + sddischarge),fill = "blue", alpha = 0.5) + 
    geom_line(aes(y=mdischarge)) + 
    scale_y_continuous("Count per day") + 
    scale_x_continuous("Day") + 
    scale_fill_manual(values = c("red","blue"),labels = c("Discharge","Death"))
  ggsave(paste0("plots/outcomes_",name,".pdf"))
  
  
  # outcomes <- melt(output$ICU_fill[,c("day","death","discharge")], id.vars = "day")
  # g2 <- ggplot(outcomes, aes(x=day,y=value)) + geom_line(aes(group = variable, colour = variable)) +
  #   scale_x_continuous("Day") + 
  #   scale_y_continuous("Number") 
  # ggsave(paste0("plots/eg_outcomes_",name,".pdf"))
}




