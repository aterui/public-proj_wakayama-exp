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
  Y <- 1 - is.na(dat$SecUL_recap)
  CL <- as.numeric(dat$cluster)
  Nday <- dat$Nday
  
  Djags <- list( Y = Y, Nsample = length(Y),
                 CL = CL, Nfl = 3, Nday = Nday )
  para <- c("xi", "p", "loglik")
  inits <- replicate(3, list(theta = runif(3, 0.005, 0.01), z = ifelse(Y==1, 1, NA),
                             .RNG.name = "base::Mersenne-Twister", .RNG.seed = NA ), simplify = F )
  for(k in 1:3) inits[[k]]$.RNG.seed <- k
  
  m <- read.jagsfile("BayesModel/model_simple.R")
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
