model{
  ninfo <- 1.0E-3

# Prior----
  logit.p0 <- logit(p0)
  logit.pi0 <- logit(pi0)
  p0 ~ dbeta(1,1)
  pi0 <- p0 + (p0*(1-p0) )
  log.mu ~ dnorm(0, ninfo)
  
  for(i in 1:2){
    tau.eps[i] ~ dscaled.gamma(2.5, 1)
    sigma.eps[i] <- 1/sqrt(tau.eps[i])
  }
    
# Likelihood----
  ## True abundance distribution----
    for(i in 1:Nsite){
      N[i] ~ dpois(lambda[i])
      log(lambda[i]) <- log.lambda[i]
      log.lambda[i] ~ dnorm(log.mu, tau.eps[1])
    }
    
  ## Removal with sequential two pass----
    for(i in 1:Nsite){
      ###p[i]: probablity of capture with one pass
      logit.p[i] ~ dnorm(logit.p0, tau.eps[2])
      logit(p[i]) <- logit.p[i]
      
      mu[i,1] <- p[i]
      mu[i,2] <- p[i]*(1-p[i])
      
      ###Proportion of individuals removed
      pi[i] <- sum(mu[i,])
      
      ###Transfer from catchability to abundance
      for(j in 1:J){
        muc[i,j] <- mu[i,j]/pi[i]
      }#j
      Y[i,1:J] ~ dmulti(muc[i,1:J], Ncap[i])
      Ncap[i] ~ dbin(pi[i], N[i])
    }#i
    
}