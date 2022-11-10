PlotIt <- function(hosts, pwdith, plength) {
  # plot hosts
  sp <- unique(hosts[, 2])
  colorss <- c("blue", "green", "black", "cyan", "purple")  #only 5 species colors
  plot(-100, -100, xlim = c(0, pwidth), ylim = c(0, plength), xlab = "Pond Length", ylab = "Pond Width")
  
  for(i in 1:length(sp)){
    dat    <- hosts[hosts[, 2] == sp[i], ]
    tads   <- dat[dat[, 4] == 1, ]
    adults <- dat[dat[, 4] == 2, ]
    points(tads[, 6], tads[, 7], pch = 21, cex =2, bg = colorss[i])
    points(adults[, 6], adults[, 7], pch = 22, cex =2, bg = colorss[i])

    tinf <- tads[tads[, 5] == 2, ]
    ainf <- adults[adults[, 5] == 2, ]
    if(length(tinf) == 7) {tinf <- t(data.frame(tinf))}
    if(length(ainf) == 7) {ainf <- t(data.frame(ainf))}
    if(length(tinf) > 0) {points(tinf[, 6], tinf[, 7], pch = 21, cex = 2, bg = colorss[i], col = "orange", lwd = 5)}
    if(length(ainf) > 0) {points(ainf[, 6], ainf[, 7], pch = 22, cex = 2, bg = colorss[i], col = "orange", lwd = 5)}
  }

  #return(population)
}