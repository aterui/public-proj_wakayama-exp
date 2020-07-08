  # Reset all
    rm(list=ls(all.names=T))
    library(tidyverse)
    
  # Data load ----
    dat <- read.csv("data/mr-data-original.csv")
    dat %>%
      mutate(id = as.character(id),
             SecUL = ceiling(distance*0.1)*10, # upstream landmark
             SecDL = SecUL - 10, # downstream landmark
             ym = format(as.Date(dat$date), format="%Y-%m"), # reformat date
             Julian = julian(as.Date(dat$date)) # Julian date
             ) -> dat
    str(dat)
    
  # Data removal ----
    ## Remove: ID of individuals that were captured once only at the last occasion
    ## Remove: individuals with mutiple treatment assignments
    dat %>%
      drop_na(cluster) %>%
      filter(id %in% names(which(table(id) > 1)) | ym != '2017-10') %>%
      filter(id %in% names(which(table(unlist(tapply(id, treatment, unique))) == 1) ) ) -> dat
  
  # History matrix ----
  ## CH: Capture history - binary information of recaptured or not
  ## DH: Location history - distance information of where individuals were caught
  ## JH: Julian history - Julian date of capture for each occasion
  
  dat %>%
    
      
    indID <- sort(unique(dat$id))
    JH <- DH <- CH <- matrix(NA, nrow = length(indID), ncol = length(YM))
    
    ## Location history matrix
    for(j in 1:length(YM)){
      tmp <- NULL
      tmp <- datL[[j]]
      for(i in 1:length(indID)){
        DH[i,j] <- ifelse(any(tmp$id==indID[i]),tmp$SecUL[tmp$id==indID[i]],NA)
      }
    }
    
    ## Capture history matrix
    for(i in 1:length(indID)){
      tmp <- NULL
      tmp <- dat[dat$id == indID[i],]
      Obs1st <- min(which(YM %in% tmp$ym) )
      CH[i, Obs1st:ncol(CH)] <- 0
      CH[i, which(YM %in% tmp$ym)] <- 1
    }
    
    ## Julian history matrix
    for(j in 1:length(YM)){
      tmp <- NULL
      tmp <- datL[[j]]
      for(i in 1:length(indID)){
        ### insert NA of Julian date if not captured
        JH[i,j] <- ifelse(any(tmp$id==indID[i]), tmp$Julian[tmp$id==indID[i]], NA)
      }
    }
    
    ## Append cluster and individual ID information
    CL <- tapply(dat$cluster, dat$id, unique)
    TR <- tapply(dat$treatment, dat$id, unique) # 1: control, 2: early, 3: late
    DH <- data.frame(indID, CL, TR, DH)
    CH <- data.frame(indID, CL, TR, CH)
    JH <- data.frame(indID, CL, TR, JH)
    
  # Save output ----
    filename1 <- paste0("Data/Matrix_DH", Sys.Date(), ".csv")
    filename2 <- paste0("Data/Matrix_CH", Sys.Date(), ".csv")
    filename3 <- paste0("Data/Matrix_JH", Sys.Date(), ".csv")
    write.csv(DH, filename1); write.csv(CH, filename2); write.csv(JH, filename3)
    