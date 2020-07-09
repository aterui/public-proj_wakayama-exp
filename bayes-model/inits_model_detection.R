# Library ----
  library(runjags)

# Data load ----
  dat <- read.csv("data/original-removal-data.csv")
  
# Data sort ----
  Y <- cbind(dat$Pass1, dat$Pass2)
  Total <- c(apply(Y,1,sum) )
  Nsite <- nrow(Y)
  J <- ncol(Y)

# MCMC setup ----
  n.ad <- 100
  n.iter <- 1E+4
  n.thin <- max(3, ceiling(n.iter/500))
  burn <- max(10, ceiling(n.iter/2))
  Sample <- ceiling(n.iter/n.thin)
  
  inits <- replicate(3, list(N = Total + 1, .RNG.name="base::Wichmann-Hill", .RNG.seed=NA), simplify = FALSE)
  for(i in 1:3){ inits[[i]]$.RNG.seed <- i + 5 }
    
# Data load for JAGS ----
  Djags <- list(Y = Y, Ncap = Total, Nsite = Nsite, J = J)
  para <- c("p0", "pi0", "logit.pi0", "sigma.eps", "log.mu", "lambda", "N")
    
  m <-	NULL
  m <- read.jagsfile(file="BayesModel/model_detectability.R")
  post <- run.jags(m$model, monitor=para, data=Djags, inits = inits, adapt = n.ad,
                   burnin = burn, sample = Sample, n.chains = 3, thin = n.thin,
                   method = "parallel", n.sims=3, modules = "glm")
        
  source("function_jags2bugs.R")
  bpost <- jags2bugs(post$mcmc)
        
# Save results ----
  filename <- paste0("Result/summary_detectability_", Sys.Date(), ".csv")
  write.csv(bpost$summary, filename)
  