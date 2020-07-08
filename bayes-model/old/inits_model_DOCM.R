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
  Nint <- dat$Julian_recap - dat$Julian
  CL <- as.numeric(dat$cluster)
  G <- abs(as.numeric(dat$MY)-2)+1
  
  Djags <- list( X = X, Y = Y, Mu = Mu, Nsample = length(X), DL = DL, UL = UL,
                 CL = CL, Ncl = 3, G = G)
  para <- c("delta", "xi", "pi")
  inits <- replicate(3, list(theta = runif(1, 0.005, 0.01), z = ifelse(Y==1, 1, NA),
                             .RNG.name = "base::Mersenne-Twister", .RNG.seed = NA ), simplify = F )
  for(k in 1:3) inits[[k]]$.RNG.seed <- k
  
  m <- read.jagsfile("BayesModel/model_DOCM.R")
  post <- run.jags(m$model, monitor = para, data = Djags,
                   n.chains = 3, inits = inits, method = "parallel",
                   burnin = burn, sample = Sample, adapt = n.ad, thin = n.thin,
                   n.sims = 3, modules = "glm")
    
  # Output
  re <- summary(post)
  