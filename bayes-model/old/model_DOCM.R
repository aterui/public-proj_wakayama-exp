model{
  ninfo <- 0.01
  
  # Priors
  theta ~ dscaled.gamma(500, 1)
  
  logit(xi) <- logit.xi
  logit.xi ~ dnorm(3.1, 1/sigma*sigma)
  sigma <- 0.45

  # Observation model
  for(i in 1:Nsample){
    Y[i] ~ dbern(z[i]*xi*pi[CL[i],G[i]])
    z[i] ~ dbern(psi[i])
    psi[i] <- (pdexp(UL[i], Mu[i], theta) - pdexp(DL[i], Mu[i], theta))/pdexp(1350, Mu[i], theta)
  }
  
  for(j in 1:Ncl){
    for(k in 1:2){
      pi[j,k] ~ dbeta(1,1)
    }
  }
  
  # Dispersal model
  for(i in 1:Nsample){
    X[i] ~ ddexp(Mu[i], theta)T(,1350)
  }
  delta <- 1/theta
  
}