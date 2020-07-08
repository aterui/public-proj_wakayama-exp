# Reset ----
  rm(list=ls(all.names=T))

# Read data ----
  library(stringr)
  dat <- read.csv("Result/re_modelCJS_ver2_2019-11-28.csv")
  
  id <- which(dat$X == "p[1,1]"):which(dat$X == "p[9,6]")
  dat_p <- dat[id,]
  gID <- str_sub(dat_p$X, start = -4, end = -4)
  tID <- str_sub(dat_p$X, start = -2, end = -2)
  dat_p$gID <- gID
  dat_p$tID <- tID
  Gname <- c("Control-CL1", "Control-CL2", "Control-CL3",
             "Early-CL1", "Early-CL2", "Early-CL3",
             "Late-CL1", "Late-CL2", "Late-CL3")
  
  pdf("figure_p.pdf", width = 10, height = 10)
  par(mfrow = c(3,3))
  sapply(1:9, function(x){
    tmp <- dat_p[dat_p$gID == x, c("X2.5.","X50.","X97.5.")]
    matplot(tmp,
            col = "black", pch = 21, bg = "white",
            type = c("l", "o", "l"), lty = c(2,1,2),
            ylim = c(min(dat_p$X2.5.), 1),
            xlab = "Julian date", ylab = "Survival",
            main = Gname[x]) })
  dev.off()