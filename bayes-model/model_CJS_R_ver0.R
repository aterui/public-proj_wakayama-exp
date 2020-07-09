model{
  # Model description:
  #   This model assumes no heterogeneity in survival among subsidy treatment and life-history groups
  #
  # Parameters:
  #   alpha - mean delta during the survey period (unit is m/per day)
  #   xi - detactability with two-pass electrofishing. Informative prior was used (estimates from removal sampling)
  #   pi - survival probability during a capture-recapture interval (duration varies by occasion)
  #   p - monthly survival
  #   mu.p - mean monthly survival
  #   sigma.p - sd of monthly survival among capture-recapture intervals
  #   phi - cumulative survival probability   
  #   theta - rate parameter for the dispersal model (Laplace)
  # Latent variables:
  #   zs - latent variable indicating whether an individidual remains in the study area or not (zs = 1, stay; zs = 0, leave)
  #   z - latent variable indicating whether an individidual survives or not (z = 1, survive; z = 0, dead)
  
  ninfo <- 0.001
  
  # Priors ----
  alpha ~ dnorm(0, ninfo)
  logit(xi) <- logit.xi
  logit.xi ~ dnorm(2.56, 1/sigma*sigma) # Informative prior
  sigma <- 0.15
  
  for(t in 1:(Nt-1)){
    logit.p[t] ~ dnorm(logit.mu.p, tau.p)
    logit(p[t]) <- logit.p[t]
    pi[t] <- exp(M[t]*log(p[t]))
    
    M[t] <- Nday[t]/30 # transform from # days to # months
  }
  logit.mu.p ~ dnorm(0, ninfo)
  logit(mu.p) <- logit.mu.p
  tau.p ~ dscaled.gamma(2.5, 1)
  sigma.p <- sqrt(1/tau.p)
  
  # Variable transformation ----
  phi[1] <- 1
  for(t in 1:(Nt-1)){
    phi[t+1] <- exp(sum(log(pi[1:t])) )
  }
  
  # CJS model ----
  for(i in 1:Nind){# Individual replicate
    zs[i,ObsF[i]] <- 1
    for(t in ObsF[i]:(Nt-1) ){# Temporal replicate
      ## Observation process
      loglik[i,t+1] <- logdensity.bern(Y[i,t+1], nu[i,t+1])
      
      Y[i,t+1] ~ dbern(nu[i,t+1])
      nu[i,t+1] <- xi*zs[i,t+1]*z[i,t+1]
      
      ## Survival process
      z[i,t+1] ~ dbern(pi[t]*z[i,t])
      
      # Dispersal model ----
      X[i,t+1] ~ ddexp(X[i,t], theta[t])T(,1350)
      zs[i,t+1] <- step(X[i,t+1])
    }
  }
  
  for(t in 1:(Nt-1)){
    theta[t] <- 1/delta[t]
    log(delta[t]) <- alpha + log(Nday[t])
  }
  
}
