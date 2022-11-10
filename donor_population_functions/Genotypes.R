Genotypes <- function(pops, parameters) {
  n.loci           <- parameters[["n.loci"]]
  n.alleles        <- parameters[["n.alleles"]]
  d.deviation      <- parameters[["dominance.deviation"]]   
  d.effect         <- parameters[["dominance.effect"]]
  n.neutral.loci   <- parameters[["n.neutral.loci"]]
  n.neutral.alleles<- parameters[["n.neutral.alleles"]]
  n.individs       <- nrow(pops)
  

  # create adaptive loci
  OUT <- NULL
  for(n in 1:n.loci){
    zeros <- rep(d.deviation, n.individs*0.5)
    pos   <- rep(d.deviation + d.effect, n.individs*0.25)
    neg   <- rep(d.deviation -d.effect, n.individs*0.25)
    out   <- c(zeros, pos, neg)
    out   <- sample(out, length(out), replace = TRUE)
    OUT <- cbind(OUT, out)
  }
  
  head(OUT)
  #hist(rowSums(OUT))

  pops <- cbind(pops, OUT)

  # create neutral loci ==========================#
  n.gtypes          <- n.individs  
  allele_freq_range <- seq(0.01, 0.99, 0.01)
  #pp                <- cbind(1, 1)
  
  OUT <- NULL  
  for (l in 1:n.neutral.loci){ 
      freq <- sample(allele_freq_range, 1)
      pp    <- freq^2
      qq    <- (1-freq)^2
      pq   <- 2*freq*(1-freq)  
      
      npp <- pp * n.gtypes
      nqq <- qq * n.gtypes
      npq <- pq * n.gtypes
      
      pp <- cbind(rep(1, npp), rep(1, npp))
      qq <- cbind(rep(2, nqq), rep(2, nqq))
      pq <- cbind(rep(1, npq), rep(2, npq))
      
      gtypes <- rbind(pp, qq, pq)
      gtype  <- gtypes[sample(1:length(gtypes[, 1]), n.gtypes, replace=TRUE),]  #replace changed from FALSE to TRUE HERE ON 3.38.16 #WHAT DOES IT DO?
            
      
      OUT <- cbind(OUT, gtype)
  }

  pops      <- cbind(pops, OUT)
  return(pops)

}
