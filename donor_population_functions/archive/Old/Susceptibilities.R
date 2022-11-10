Susceptibilities <- function(hosts, host.suscepts) {
  gtypes <- unique(hosts[, 3])
  #samp   <- sample(seq(host.suscepts[1], host.suscepts[2], 0.001), length(gtypes), replace = TRUE)  # uniform dist with replacement; needed if each individual has unique genotype
  
  #normal
  #mean.norm <- mean(c(host.suscepts[1], host.suscepts[2]))
  #hist(sample(rnorm(500000, mean.norm, (host.suscepts[2]-host.suscepts[1])/10), length(gtypes), replace = TRUE))
  #samp <- sample(rnorm(500000, mean.norm, (host.suscepts[2]-host.suscepts[1])/10), length(gtypes), replace = TRUE)
  #samp[samp < 0] <- 0
  #samp[samp > 1] <- 1

  #log normal
  #hist(rlnorm(500000, meanlog = -.7, sdlog = (.6-.4)/2), breaks = 50)
  samp <- rlnorm(500000, meanlog = -0.7, sdlog = (host.suscepts[2] - host.suscepts[1])/2)
  #samp <- rlnorm(500000, meanlog = -0.7, sdlog = (1 - 0)/2)
  remo <- which(samp > 1)
  if(length(remo > 0)){samp <- samp[-remo]}
  samp[samp < 0] <- 0
  samp[samp > 1] <- 1
  #hist(samp, breaks = 50)
  samp <- sample(samp, length(gtypes))


  susceptibilities <- cbind(gtypes, samp)
  return(susceptibilities)
}  