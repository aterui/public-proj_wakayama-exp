# Reset ----
  rm(list=ls(all.names=T))

# Read data ----
  library(stringr)
  dat <- read.csv("Result/re_modelCJS_R_ver2_2019-12-09.csv")
  JH <- read.csv("Data/Matrix_JH2019-11-24.csv")
  J <- as.matrix(JH[,which(colnames(JH)=="X1"):ncol(JH)])
  JulianDate <- apply(J, 2, median, na.rm = T)
  
  dat_phi <- dat[str_detect(dat$X, "phi"),]
  gID <- str_sub(dat_phi$X, start = -4, end = -4)
  tID <- str_sub(dat_phi$X, start = -2, end = -2)
  dat_phi$gID <- gID
  dat_phi$tID <- tID
  Gname <- c("Control-CL1", "Control-CL2", "Control-CL3",
             "Early-CL1", "Early-CL2", "Early-CL3",
             "Late-CL1", "Late-CL2", "Late-CL3")

# Plot  
  pdf("figure_phi_R_ver2.pdf", width = 10, height = 10)
  par(mfrow = c(3,3))
  sapply(1:9, function(x){
              tmp <- dat_phi[dat_phi$gID == x, c("X2.5.","X50.","X97.5.")]
              matplot(JulianDate, tmp,
                      col = "black", pch = 21, bg = "white",
                      type = c("l", "o", "l"), lty = c(2,1,2),
                      ylim = c(0, 1),
                      xlab = "Julian date", ylab = "Cumulative survival",
                      main = Gname[x]) })
            
  dev.off()