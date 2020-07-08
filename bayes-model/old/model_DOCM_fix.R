model{
  ninfo <- 0.01
  
  # Priors
  for(k in 1:2){
    b[k] ~ dnorm(0, ninfo)
  }
  
  p ~ dunif(0,1)
  logit(xi) <- logit.xi
  logit.xi ~ dnorm(3.1, 1/sigma*sigma)
  sigma <- 0.45
  
  # Observation model
  for(i in 1:Nsample){
    loglik[i] <- logdensity.bern(Y[i], z[i]*xi*pi[i])
    Y[i] ~ dbern(z[i]*xi*pi[i])
    z[i] ~ dbern(psi[i])
    psi[i] <- (pdexp(UL[i], Mu[i], theta[i]) - pdexp(DL[i], Mu[i], theta[i]))/pdexp(1350, Mu[i], theta[i])
    pi[i] <- pow(p, Nday[i]/365)
  }
  
  # Dispersal model
  for(i in 1:Nsample){
    X[i] ~ ddexp(Mu[i], theta[i])T(,1350)
    theta[i] <- 1/delta[i]
    log(delta[i]) <- b[1] + b[2]*FL[i] + log(Nday[i])
  }
  
}
