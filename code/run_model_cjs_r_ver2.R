
# setup -------------------------------------------------------------------

rm(list=ls(all.names=T))
pacman::p_load(tidyverse, runjags, loo)


# read data ---------------------------------------------------------------

DH <- readRDS("data_fmt/matrix_dh.rds")
CH <- readRDS("data_fmt/matrix_ch.rds")
JH <- readRDS("data_fmt/matrix_jh.rds")
J <- as.matrix(JH[,which(colnames(JH) == "occasion1"):ncol(JH)])


# mcmc setting ------------------------------------------------------------

n.ad <- 100
n.iter <- 2E+4
n.thin <- max(3, ceiling(n.iter/500))
burn <- ceiling(max(10, n.iter/2))
Sample <- ceiling(n.iter/n.thin)


# bayesian inference ------------------------------------------------------

## Data for JAGS
### Response
X <- as.matrix(DH[,which(colnames(DH) == "occasion1"):ncol(DH)] - 5)
Y <- z <- as.matrix(CH[,which(colnames(CH) == "occasion1"):ncol(CH)])
z[Y == 0] <- NA
ObsF <- apply(Y, 1, function(x) min(which(is.na(x) == 0)))
ObsL <- apply(Y, 1, function(x) max(which(x == 1)))

for(i in 1:length(ObsF)){
  z[i,ObsF[i]:ObsL[i]] <- 1
}

### Explanatory
Nday <- diff(apply(J, 2, median, na.rm = T))
Grid <- expand.grid(CL = 1:3, TR = 1:3)
Grid$G <- 1:9
Grid$id <- as.numeric(paste0(Grid$CL, Grid$TR) )
groupdat <- data.frame(id = as.numeric(paste0(CH$CL, CH$TR))) %>%
  left_join(Grid, by = "id")
G <- groupdat$G

Djags <- list(X = X,
              Y = Y,
              Nday = Nday,
              z = z,
              Nind = nrow(Y),
              Nt = ncol(Y),
              Ng = length(unique(G)),
              G = G,
              ObsF = ObsF)

para <- c("xi", "mu.p", "sigma.p", "p", "pi", "phi", "alpha")
inits <- replicate(3,
                   list(logit.p = matrix(0, nrow = 9, ncol = 6),
                        .RNG.name = "base::Mersenne-Twister",
                        .RNG.seed = NA ),
                   simplify = F)
for(k in 1:3) inits[[k]]$.RNG.seed <- k

m <- read.jagsfile("code/model_cjs_r_ver2.R")
post <- run.jags(m$model,
                 monitor = para,
                 data = Djags,
                 n.chains = 3,
                 inits = inits,
                 method = "parallel",
                 burnin = burn,
                 sample = Sample,
                 adapt = n.ad,
                 thin = n.thin,
                 n.sims = 3,
                 modules = "glm")


# output ------------------------------------------------------------------

bpost <- MCMCvis::MCMCsummary(post$mcmc)
saveRDS(bpost, "result/re_model_cjs.rds")
print(max(na.omit(bpost[, "Rhat"])))
