library(rjags)
library(R2WinBUGS)
 
jags2bugs <- function(mcmc.list)
{
   b1 <- mcmc.list[[1]]
   m1 <- as.matrix(b1)
   mall <- matrix(numeric(0), 0, ncol(m1))
   n.chains <- length(mcmc.list)
   for (i in 1:n.chains) {
      mall <- rbind(mall, as.matrix(mcmc.list[[i]]))
   }
   sims.array <- array(mall, dim = c(nrow(m1), n.chains, ncol(m1)))
   dimnames(sims.array) <- list(NULL, NULL, colnames(m1))
   mcpar <- attr(b1, "mcpar")
   as.bugs.array(
      sims.array = sims.array,
      model.file = NULL,
      program = NULL,
      DIC = TRUE,
      DICOutput = NULL,
      n.iter = mcpar[2],
      n.burnin = mcpar[1] - mcpar[3],
      n.thin = mcpar[3]
   )
}