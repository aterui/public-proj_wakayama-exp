# Function ----
  library(runjags)
  dat <- read.csv("Data/Data_Convert2019-11-08.csv")
  dat <- dat[is.na(dat$cluster)==0, ]
  
# MCMC setting ----
  n.ad <- 100
  n.iter <- 3E+3
  n.thin <- max(3, ceiling(n.iter/500))
  burn <- ceiling(max(10, n.iter/2))
  Sample <- ceiling(n.iter/n.thin)

# Bayesian Inference ----  
  ## Data for JAGS
  X <- dat$SecDL_recap + 5
  Mu <- dat$SecDL + 5
  Y <- 1 - is.na(X)
  UL <- ifelse(is.na(X), 1350, dat$SecUL_recap)
  DL <- ifelse(is.na(X), 0, dat$SecDL_recap)
  
  CL <- as.numeric(dat$cluster)
  FL <- c(scale(dat$fl))
  Nday <- ifelse(is.na(dat$Julian_recap), dat$Nday, dat$Julian_recap-dat$Julian)
  
  Djags <- list( X = X, Y = Y, Mu = Mu, Nsample = length(X), DL = DL, UL = UL,
                 FL = FL, CL = CL, Nfl = 3, Nday = Nday )
  para <- c("xi", "p", "b", "loglik")
  inits <- replicate(3, list(b = c(3,0), z = ifelse(Y==1, 1, NA),
                             .RNG.name = "base::Mersenne-Twister", .RNG.seed = NA ), simplify = F )
  for(k in 1:3) inits[[k]]$.RNG.seed <- k
  
  m <- read.jagsfile("BayesModel/model_DOCM_CL.R")
  post <- run.jags(m$model, monitor = para, data = Djags,
                   n.chains = 3, inits = inits, method = "parallel",
                   burnin = burn, sample = Sample, adapt = n.ad, thin = n.thin,
                   n.sims = 3, modules = "glm")
    
  # Output
  re <- summary(post)
  library(loo)
  loglik <- sapply(1:length(Y), function(i) unlist( post$mcmc[, paste0("loglik[", i, "]") ] ) )
  WAIC <- waic(loglik)
  WAIC
  