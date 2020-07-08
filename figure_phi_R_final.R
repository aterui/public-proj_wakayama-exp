# Reset ----
  rm(list=ls(all.names=T))

# Read data ----
  library(stringr)
  dat <- read.csv("Result/re_modelCJS_R_ver2_2019-12-09.csv")
  ID <- which(dat$X=="phi[1,7]"):which(dat$X=="phi[9,7]")
  dat_phi <- dat[ID,]
  L <- list(NULL); L[[1]] <- 1:3; L[[2]] <- 4:6; L[[3]] <- 7:9
  TR <- c("Control", "Early", "Late")
  xlab <- c("CL1", "CL2", "CL3")
  
# Plot  
  pdf("figure_phi_R_final.pdf", width = 10, height = 4)
  par(mfrow = c(1,3), cex.axis = 1.5, oma = c(2,2,0,0))
    for(i in 1:length(L)){
      plot(0, xlim= c(0.5, 3.5), ylim = c(0,1), type = "n", ann = F, axes = F)
      points(dat_phi[L[[i]],"X50."], pch = 19, cex = 1.5)
      segments(1:3, dat_phi[L[[i]],"X2.5."], 1:3, dat_phi[L[[i]],"X97.5."])
      axis(1, at = 1:3, labels = xlab); axis(2, las = 2); box(bty = "l")
      mtext(TR[i])
    }
  mtext("Growth cluster", side = 1, outer = T)
  mtext("Survival", side = 2, outer = T)
  dev.off()