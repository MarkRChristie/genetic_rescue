AdditiveVariation <- function(pops, parameters) {
  d.effect    <- parameters[["dominance.effect"]]
  d.deviation <- parameters[["dominance.deviation"]]
  #phenotypes  <- pops[, (ncol(pops)-n.loci+1):ncol(pops)]
  dims        <- ncol(pops)-(n.neutral.loci*2)-n.loci # seprate out adaptive loci first
  phenotypes  <- pops[, (dims+1):(dims+n.loci)]

  variation <- function(x){length(unique(x))}
  fixed     <- apply(phenotypes, 2, variation)
  fixed     <- which(fixed == 1) 
  
  if(length(fixed) > 0){
    new.d.dev <- length(fixed)*d.effect  # assumes positive selection only so far!!!
    pops[, 8] <- pops[1, 8] + new.d.dev 
    n.individs<- nrow(pops) 
  
    OUT <- NULL
    for(n in 1:length(fixed)){
      zeros <- rep(d.deviation, ceiling(n.individs*0.5))
      pos   <- rep(d.deviation + d.effect, n.individs*0.25)
      neg   <- rep(d.deviation - d.effect, n.individs*0.25)
      out   <- c(zeros, pos, neg)
      out   <- sample(out, length(out), replace = TRUE)
      OUT   <- cbind(OUT, out)
    }
    
    all.cols  <- (dims+1):(dims+n.loci)  # all cols with genotypes
    new.cols  <- all.cols[fixed]
    pops[1:length(OUT[, 1]), new.cols] <- OUT  # rows indexed because occaisonally 1 short due to rounding 
    
    }
    
  return(pops)
    
}
