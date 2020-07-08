# Reset ----
  rm(list=ls(all.names=T))

# Read data ----
  library(stringr)
  dat <- read.csv("Result/re_CJS_2019-11-19.csv")
  JH <- read.csv("Data/Matrix_JH2019-11-09.csv")
  J <- as.matrix(JH[,which(colnames(JH)=="X1"):ncol(JH)])
  JulianDate <- apply(J, 2, median, na.rm = T)
  
  PHI <- list(NULL)
  PHI[[1]] <- dat[str_detect(dat$X, "phi1"),]
  PHI[[2]] <- dat[str_detect(dat$X, "phi2"),]
  PHI[[3]] <- dat[str_detect(dat$X, "phi3"),]
  
  pdf("figure_survival.pdf", width = 10, height = 3.5)
  par(mfrow = c(1,3))
  lapply(1:3, function(x){
        matplot(JulianDate, PHI[[x]][,c("X2.5.","X50.","X97.5.")],
                col = "black", pch = 21, bg = "white",
                type = c("l", "o", "l"), lty = c(2,1,2),
                ylim = c(0, 1),
                xlab = "Julian date", ylab = "Survival",
                main = paste0("Cluster ", x)) })
  
  dev.off()