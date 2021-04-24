
# setup -------------------------------------------------------------------

  rm(list=ls(all.names = T))
  pacman::p_load(tidyverse, runjags, loo)


# read data ---------------------------------------------------------------

  DH <- read_csv("data/matrix_dh2021-04-23.csv")
  CH <- read_csv("data/matrix_ch2021-04-23.csv")
  JH <- read_csv("data/matrix_jh2021-04-23.csv")
  J <- as.matrix(JH[,which(colnames(JH) == "occasion1"):ncol(JH)])

  
# mcmc setting ------------------------------------------------------------
  
  n.ad <- 100
  n.iter <- 1E+4
  n.thin <- max(3, ceiling(n.iter/500))
  burn <- ceiling(max(10, n.iter/2))
  Sample <- ceiling(n.iter/n.thin)


# bayesian inference ------------------------------------------------------
  
  ## Data for JAGS
  X <- as.matrix(DH[,which(colnames(DH) == "occasion1"):ncol(DH)] - 5)
  Y <- z <- as.matrix(CH[,which(colnames(CH) == "occasion1"):ncol(CH)])
  z[Y == 0] <- NA
  ObsF <- apply(Y, 1, function(x)min(which(is.na(x) == 0)))
  ObsL <- apply(Y, 1, function(x)max(which(x == 1)))
  
  for(i in 1:length(ObsF)) {
    z[i,ObsF[i]:ObsL[i]] <- 1
  }
  
  Nday <- diff(apply(J, 2, median, na.rm = T))
  CL <- CH$CL

  Djags <- list(X = X,
                Y = Y,
                Nday = Nday,
                z = z,
                Nind = nrow(Y),
                Nt = ncol(Y),
                Ncl = length(unique(CL)),
                CL = CL,
                ObsF = ObsF)
  
  para <- c("xi", "mu.p", "sigma.p", "p", "pi", "phi", "alpha", "loglik")
  inits <- replicate(3,
                     list(logit.p = matrix(2, nrow = 3, ncol = 6),
                          .RNG.name = "base::Mersenne-Twister",
                          .RNG.seed = NA),
                     simplify = F)
  for(k in 1:3) inits[[k]]$.RNG.seed <- k
  
  m <- read.jagsfile("bayes-model/model_cjs_r_ver1.R")
  post <- run.jags(m$model,
                   monitor = para,
                   data = Djags,
                   n.chains = 3,
                   inits = inits,
                   method = "parallel",
                   burnin = burn,
                   sample = Sample,
                   adapt = n.ad,
                   thin = n.thin,
                   n.sims = 3,
                   modules = "glm")


# output ------------------------------------------------------------------
    
  bpost <- MCMCvis::MCMCsummary(post$mcmc)
  file1 <- paste0("result/re_model_cjs_r_ver1_", Sys.Date(), ".csv")
  file2 <- paste0("result/waic_model_cjs_r_ver1_", Sys.Date(), ".csv")
  maxid <- min(which(str_detect(rownames(bpost), "loglik"))) - 1
  print(max(bpost[1:maxid, "Rhat"]) )
  
  if(all(na.omit(bpost$Rhat[1:maxid]) < 1.1)){
    ## Estimate summary
    write.csv(bpost, file1)
    
    ## WAIC
    loglik <- NULL
    for(i in 1:nrow(Y)) {
      for(t in (ObsF[i]+1):ncol(Y)) {
        x <- unlist(post$mcmc[,paste0("loglik[", i, ",", t, "]")])
        loglik <- cbind(loglik, c(x))
      }
    }
    WAIC <- waic(loglik)
    write.csv(WAIC$estimates, file2)
  }