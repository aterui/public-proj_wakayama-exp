# Reset all
  rm(list=ls(all.names=T))

# Data load ----
  dat <- read.csv("Data/Data_MarkRecapture_original.csv")
  dat$id <- as.character(dat$id)
  dat$SecUL <- ceiling(dat$distance*0.1)*10 # upstream landmark
  dat$SecDL <- dat$SecUL - 10 # downstream landmark
  dat$ym <- format(as.Date(dat$date), format="%Y-%m")
  dat$Julian <- julian(as.Date(dat$date))
  print(nrow(dat))
  
# Data removal ----
  ## remove individual with no cluster data
  dat <- dat[is.na(dat$cluster)==0,] ## remove individuals with no cluster info
  print(nrow(dat))
  
  ## remove individual that were captured once only at the last occasion
  indID_single <- names(which(table(dat$id)==1)) # individual ID with only-one capture 
  indID_last <- unique(dat$id[dat$ym=="2017-10"]) # individual ID captured at last occasion 
  indID_sl <- intersect(indID_single, indID_last) # intersect
  dat <- dat[-which(dat$id %in% indID_sl), ] # remove individuals
  print(nrow(dat))
  
  ## remove individuals with mutiple treatment assignments
  treat_u <- tapply(dat$treatment, dat$id, unique)
  n_treat <- lapply(treat_u, length)
  indID_mt <- names(which(n_treat > 1) ) # indID for individuals with multiple treatments
  dat <- dat[-which(dat$id %in% indID_mt),]
  print(nrow(dat))
  
# Data selection ----
  ## Check unnecessary duplicates
  datL <- list(NULL)
  YM <- sort(unique(dat$ym))
  for(i in 1:length(YM)){
    datL[[i]] <- dat[dat$ym == YM[i],]
  }

  ## check individualID duplicate for each month
  which(lapply(datL, function(x)any(table(x$id)==2) ) == 1)
  
  ## remove duplicate [necessary if individuals with no cluster info were not removed]
  #dupID <- names(which(table(datL[[2]]$id)==2)) #"0YBR"
  #datL[[2]] <- datL[[2]][-which(datL[[2]]$id==dupID)[2],] # remove duplicate
  #any(table(datL[[2]]$id)==2) # check data
  
# History matrix ----
## CH: Capture history - binary information of recaptured or not
## DH: Location history - distance information of where individuals were caught
## JH: Julian history - Julian date of capture for each occasion
  
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
  