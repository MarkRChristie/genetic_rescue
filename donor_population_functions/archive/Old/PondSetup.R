PondSetup <- function(plength, pwidth, n.species, n.genotypes, n.adults, n.tads) {

  total.n <- (n.adults + n.tads) * n.species

  x.positions <- seq(from = 0, to = plength, by = plength/10000) # determine resolution (pixel density) of pond
  y.positions <- seq(from = 0, to = pwidth, by = pwidth/10000)   # determine resolution (pixel density) of pond
  
  x.pos <- sample(x.positions, total.n, replace = TRUE)  
  y.pos <- sample(y.positions, total.n, replace = TRUE) 

  id <- 1:total.n # create individual identifier
  species <- sort(rep(1:n.species, total.n/n.species))
  gtypes  <- sort(rep(1:(n.genotypes*n.species), total.n/n.species/n.genotypes))  # must be divsibile numbers or will throw an error
  stage   <- rep(sample(rep(1:2, c(n.tads, n.adults))), n.species)  # 1 = tadpole, 2= adult; sample is used to shufle adults and tadpoles across genotypes
  infect  <- rep(1, total.n) # all are uninfected at start

  population <- cbind(id, species, gtypes, stage, infect, x.pos, y.pos)
  #population[1, 5] <- 2               # create first infected individual
  
  return(population)
}


