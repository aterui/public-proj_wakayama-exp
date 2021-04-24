#' ---
#' title: "Data sorting: Wakayama experiment"
#' author: Akira Terui
#' output:
#'   html_document:
#'     toc: TRUE
#'     toc_float: TRUE
#'     theme: "paper"
#'     df_print: paged
#' ---

#' # Session information
  
  sessionInfo()

#' # Load library
#' Initialize and load `tidyverse`
  
  # Reset all
  rm(list=ls(all.names=T))
  library(tidyverse)
  
#' # Read data
  
  # Data load ----
  
  d0 <- read_csv("data/original-mr-data.csv")
  dat <- d0 %>%
    mutate(id = as.character(id),
           SecUL = ceiling(distance * 0.1) * 10, # upstream landmark
           SecDL = SecUL - 10, # downstream landmark
           date = paste(year, month, day, sep = "-"),
           ym = format(as.Date(date), format="%Y-%m"), # reformat date
           Julian = julian(as.Date(date))) %>% # Julian date
    select(-X1, -X.1, -X, -No., -No)

#' # Data removal
#' 
#' - Remove: ID of individuals that were captured once only at the last occasion
#' - Remove: individuals with mutiple treatment assignments
  
  # Data removal ----
  
  ## Remove: ID of individuals that were captured once only at the last occasion
  ## Remove: individuals with multiple treatment assignments
  dat1 <- dat %>%
    drop_na(cluster) %>%
    filter(id %in% names(which(table(id) > 1)) | ym != '2017-10') %>%
    filter(id %in% names(which(table(unlist(tapply(id, treatment, unique))) == 1))) %>% 
    mutate(Occasion = paste0("occasion", as.numeric(factor(ym))))
           
#' # History matrix
#' Create matrices of capture-history `CH`, capture locations `DH`, Julian date `JH`
#' 
#' - Variable transformation
#'     - `Occasion` - date (`ym`) transformation: `1...7 = 2016-05...2017-10`
#'     - `CL` - `cluster` transformation: `c(1, 2, 3) = c('1st', '2nd', '3rd')`
#'     - `TR` - `treatment` transformation: `c(1, 2, 3) = c('control', 'early', 'late')`
  
  # History matrix ----
  
  ## CH: Capture history - binary information of recaptured or not
  CH <- dat1 %>%
    mutate(capture = 1) %>%
    arrange(Occasion) %>%
    pivot_wider(names_from = Occasion,
                values_from = capture,
                id_cols = c(id, cluster, treatment),
                values_fill = list(capture = 0)) %>%
    rename(CL = cluster) %>%
    mutate(CL = as.numeric(factor(CL),
                           levels = c("1st",
                                      "2nd",
                                      "3rd")
                           )
           ) %>%
    rename(TR = treatment) %>%
    mutate(TR = as.numeric(factor(TR,
                                  levels = c("control",
                                             "early",
                                             "late")
                                  )
                           )
           ) %>% # 1: control, 2: early, 3: late
    arrange(id)
  
  ## put NA until the first capture
  ch <- CH %>%
    select(sort(unique(dat1$Occasion)))
  
  for(i in 1:nrow(ch)) {
    x <- min(which(ch[i,] == 1)) - 1
    if(x != 0) ch[i, 1:x] <- NA
  }
  
  CH[, which(colnames(CH) %in% sort(unique(dat1$Occasion)))] <- ch
  
  print(CH)
  
  ## DH: Location history - distance from the downstream end to the upstream landmark of a (re)capture subsection
  DH <- dat1 %>%
    arrange(Occasion) %>%
    pivot_wider(names_from = Occasion,
                values_from = SecUL,
                id_cols = c(id, cluster, treatment)) %>%
    rename(CL = cluster) %>%
    mutate(CL = as.numeric(factor(CL),
                           levels = c("1st",
                                      "2nd",
                                      "3rd")
                           )
           ) %>%
    rename(TR = treatment) %>%
    mutate(TR = as.numeric(factor(TR,
                                  levels = c("control",
                                             "early",
                                             "late")
                                  )
                           )
           ) %>% # 1: control, 2: early, 3: late
    arrange(id)
  
  print(DH)
  
  ## JH: Julian history - Julian date of capture for each occasion
  JH <- dat1 %>%
    arrange(Occasion) %>%
    pivot_wider(names_from = Occasion,
                values_from = Julian,
                id_cols = c(id, cluster, treatment)) %>%
    rename(CL = cluster) %>%
    mutate(CL = as.numeric(factor(CL),
                           levels = c("1st",
                                      "2nd",
                                      "3rd")
                           )
           ) %>%
    rename(TR = treatment) %>%
    mutate(TR = as.numeric(factor(TR,
                                  levels = c("control",
                                             "early",
                                             "late")
                                  )
                           )
           ) %>% # 1: control, 2: early, 3: late
    arrange(id)
  
  print(JH)

#' # Save files
#' Save matrices
#+ eval = F 
  # Save output ----
  filename1 <- paste0("data/matrix_dh", Sys.Date(), ".csv")
  filename2 <- paste0("data/matrix_ch", Sys.Date(), ".csv")
  filename3 <- paste0("data/matrix_jh", Sys.Date(), ".csv")
  write.csv(DH, filename1); write.csv(CH, filename2); write.csv(JH, filename3)
