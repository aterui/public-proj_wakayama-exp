model{
  ninfo <- 0.01
  
  # Priors
  for(j in 1:Nfl){
    p[j] ~ dbeta(1,1)
    theta[j] ~ dscaled.gamma(500,1)
    delta[j] <- 1/theta[j]
  }
  
  logit(xi) <- logit.xi
  logit.xi ~ dnorm(3.1, 1/sigma*sigma)
  sigma <- 0.45
  
  # Observation model
  for(i in 1:Nsample){
    loglik[i] <- logdensity.bern(Y[i], xi*pi[i])
    Y[i] ~ dbern(0.95*pi[i])
    pi[i] <- pow(p[CL[i]], Nday[i]/365)
  }

}
