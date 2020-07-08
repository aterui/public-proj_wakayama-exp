model{
  ninfo <- 0.001
  
  # Priors ----
  alpha ~ dnorm(0, ninfo)
  logit(xi) <- logit.xi
  logit.xi ~ dnorm(2.56, 1/sigma*sigma) # Informative prior
  sigma <- 0.15

  for(t in 1:(Nt-1)){
    for(j in 1:Ncl){
      logit(pi[j,t]) <- logit.pi[j,t]
      logit.pi[j,t] ~ dnorm(logit.mu.pi, tau.pi)
      p[j,t] <- exp(log(pi[j,t])/(Nday[t]/30) )
    }
  }
  logit.mu.pi ~ dnorm(0, ninfo)
  logit(mu.pi) <- logit.mu.pi
  tau.pi ~ dscaled.gamma(2.5, 1)
  sigma.pi <- 1/sqrt(tau.pi)
  
  # Variable transformation ----
  for(j in 1:Ncl){
    phi[j,1] <- 1
    for(t in 1:(Nt-1)){
      phi[j,t+1] <- exp(sum(log(pi[j,1:t])) )
    }
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
      z[i,t+1] ~ dbern(pi[CL[i],t]*z[i,t])
  
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
