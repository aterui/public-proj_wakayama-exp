  # Reset all
  rm(list=ls(all.names=T))
  library(tidyverse)
  
  # Data load ----
  d0 <- read.csv("data/mr-data-original.csv")
  dat = d0 %>%
    mutate(id = as.character(id),
           SecUL = ceiling(distance*0.1)*10, # upstream landmark
           SecDL = SecUL - 10, # downstream landmark
           ym = format(as.Date(date), format="%Y-%m"), # reformat date
           Julian = julian(as.Date(date)) # Julian date
    )
  str(dat)
  
  # Data removal ----
  ## Remove: ID of individuals that were captured once only at the last occasion
  ## Remove: individuals with mutiple treatment assignments
  dat1 = dat %>%
    drop_na(cluster) %>%
    filter(id %in% names(which(table(id) > 1)) | ym != '2017-10') %>%
    filter(id %in% names(which(table(unlist(tapply(id, treatment, unique))) == 1) ) )

  # History matrix ----
  ## CH: Capture history - binary information of recaptured or not
  CH = dat1 %>%
    mutate(Occasion = as.numeric(factor(ym) ), capture = 1) %>%
    arrange(Occasion) %>%
    pivot_wider(names_from = Occasion, values_from = capture, id_cols = c(id, cluster, treatment), values_fill = list(capture = 0)) %>%
    rename(CL = cluster) %>%
    mutate(CL = as.numeric(CL)) %>%
    rename(TR = treatment) %>%
    mutate(TR = as.numeric(TR)) %>% # 1: control, 2: early, 3: late
    arrange(id)

  ch = CH %>% select(as.character(1:7))
  for(i in 1:nrow(ch)){
    x <- min(which(ch[i,] == 1)) - 1
    if(x != 0) ch[i,1:x] <- NA
  }
  
  CH[,which(colnames(CH) %in% as.character(1:7))] <- ch

  ## DH: Location history - distance information of where individuals were caught
  DH = dat1 %>%
    mutate(Occasion = as.numeric(factor(ym) )) %>%
    arrange(Occasion) %>%
    pivot_wider(names_from = Occasion, values_from = SecUL, id_cols = c(id, cluster, treatment)) %>%
    rename(CL = cluster) %>%
    mutate(CL = as.numeric(CL)) %>%
    rename(TR = treatment) %>%
    mutate(TR = as.numeric(TR)) %>% # 1: control, 2: early, 3: late
    arrange(id)
  
  ## JH: Julian history - Julian date of capture for each occasion
  JH = dat1 %>%
    mutate(Occasion = as.numeric(factor(ym) )) %>%
    arrange(Occasion) %>%
    pivot_wider(names_from = Occasion, values_from = Julian, id_cols = c(id, cluster, treatment)) %>%
    rename(CL = cluster) %>%
    mutate(CL = as.numeric(CL)) %>%
    rename(TR = treatment) %>%
    mutate(TR = as.numeric(TR)) %>% # 1: control, 2: early, 3: late
    arrange(id)
  
  # Save output ----
  filename1 <- paste0("data/matrix_dh", Sys.Date(), ".csv")
  filename2 <- paste0("data/matrix_ch", Sys.Date(), ".csv")
  filename3 <- paste0("data/matrix_jh", Sys.Date(), ".csv")
  write.csv(DH, filename1); write.csv(CH, filename2); write.csv(JH, filename3)
