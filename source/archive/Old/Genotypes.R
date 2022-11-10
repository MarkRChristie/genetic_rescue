Genotypes <- function(pops, n.loci, n.alleles) {
  n.gtypes <- length(pops[, 1])
  n.loci   <- parameters[["n.loci"]]
  n.alleles<- parameters[["n.alleles"]]

  col1 <- rep(0, n.gtypes)
  col2 <- rep(0, n.gtypes)   
  #output    <- cbind(1:n.gtypes, output) # i is the current iteration
  pops <- cbind(pops, col1, col2)
}
