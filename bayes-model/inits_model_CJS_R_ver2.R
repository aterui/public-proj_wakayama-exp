# Reset ----
  rm(list=ls(all.names=T))

# Read data ----
  library(runjags)
  DH <- read.csv("data/matrix_dh2020-07-08.csv")
  CH <- read.csv("data/matrix_ch2020-07-08.csv")
  JH <- read.csv("data/matrix_jh2020-07-08.csv")
  J <- as.matrix(JH[,which(colnames(JH)=="X1"):ncol(JH)])
  
# MCMC setting ----
  n.ad <- 100
  n.iter <- 2E+3
  n.thin <- max(3, ceiling(n.iter/500))
  burn <- ceiling(max(10, n.iter/2))
  Sample <- ceiling(n.iter/n.thin)

# Bayesian Inference ----  
  ## Data for JAGS
  ### Response
  X <- as.matrix(DH[,which(colnames(DH)=="X1"):ncol(DH)] - 5)
  Y <- z <- as.matrix(CH[,which(colnames(CH)=="X1"):ncol(CH)])
  z[Y==0] <- NA
  ObsF <- apply(Y, 1, function(x)min(which(is.na(x)==0) ) )
  ObsL <- apply(Y, 1, function(x)max(which(x==1)) )
  for(i in 1:length(ObsF)){ z[i,ObsF[i]:ObsL[i]] <- 1 }
  
  ### Explanatory
  Nday <- diff(apply(J, 2, median, na.rm = T))
  Grid <- expand.grid(CL = 1:3, TR = 1:3)
  Grid$G <- 1:9; Grid$id <- as.numeric(paste0(Grid$CL, Grid$TR) )
  groupdat = data.frame(id = as.numeric(paste0(CH$CL, CH$TR)) ) %>% left_join(Grid, by = "id")
  G <- groupdat$G

  Djags <- list( X = X, Y = Y, Nday = Nday, z = z,
                 Nind = nrow(Y), Nt = ncol(Y), Ng = length(unique(G) ),
                 G = G, ObsF = ObsF)
  
  para <- c("mu.pi", "sigma.pi", "xi", "alpha", "p", "phi", "loglik")
  inits <- replicate(3, list(logit.pi = matrix(0, nrow = 9, ncol = 6),
                             .RNG.name = "base::Mersenne-Twister",
                             .RNG.seed = NA ), simplify = F )
  for(k in 1:3) inits[[k]]$.RNG.seed <- k
  
  m <- read.jagsfile("bayes-model/model_cjs_r_ver2.R")
  post <- run.jags(m$model, monitor = para, data = Djags,
                   n.chains = 3, inits = inits, method = "parallel",
                   burnin = burn, sample = Sample, adapt = n.ad, thin = n.thin,
                   n.sims = 3, modules = "glm")
  
# Output ----
  source("function_jags2bugs.R")
  bpost <- jags2bugs(post$mcmc)
  file1 <- paste0("Result/re_model_cjs_r_ver2_", Sys.Date(), ".csv")
  write.csv(bpost$summary, file1)
  print(bpost, 2)
  
# WAIC ----
  library(loo)
  loglik <- NULL
  for(i in 1:nrow(Y)){
    for(t in (ObsF[i]+1):ncol(Y)){
      x <- unlist(post$mcmc[,paste0("loglik[", i, ",", t, "]")])
      loglik <- cbind(loglik, c(x) )
    }
  }
  WAIC <- waic(loglik)
  file2 <- paste0("result/waic_model_cjs_r_ver2_", Sys.Date(), ".csv")
  write.csv(WAIC$estimates, file2)
  