Selection <- function(pops, parameters) {
  # as written right now, selection acts on all individuals
  # if changed in future, good to think about WHEN selection occurs (before/after change in stage)
  #juveniles <- pops[pops[, 4] == 1, ]
  #adults    <- pops[pops[, 4] == 2, ]
  n.loci                <- parameters[["n.loci"]]
  n.neutral.loci        <- parameters[["n.neutral.loci"]]
  #selection.strength    <- parameters[["selection.strength"]] #proportion that die  
  environmental.effect  <- parameters[["environmental.efffect"]] # strength of environmental contribution to phenotype
  optima                <- parameters[["optima"]] # phenotypic optimum in new environmnet (e.g., temp tolerance of +2 degrees c)
  

  # fitness function
  # stabilizing selection only
  #x <- seq(from=-2.5, to = 2.5, by = 0.001)
  
  # stabilizing selection
  #curve(-x^2+2, col = "violet", -2, 2)
  #y <- -x^2+15
  #plot(x, y, xlim = c(-1, 1))  # below keeps individuals essentially + or - 1 degree
  
  
  # calculate phenotype values
  dims        <- ncol(pops)-(n.neutral.loci*2)-n.loci # seprate out adaptive loci first
  phenotypes  <- pops[, (dims+1):(dims+n.loci)]

  #phenotypes <- pops[, (ncol(pops)-n.loci+1):ncol(pops)]
  phenotypes <- rowSums(phenotypes) + pops[, 8]   # ADDING IN d.dev from pops HERE
  
  # add environmental variation
  OUT <- NULL
  for(p in 1:length(phenotypes)){
    ptype <- phenotypes[p]
    ptype <- rnorm(1, ptype, environmental.effect) # subract 1 from ptype if want environment to counter phenotype
    OUT   <- c(OUT, ptype)
  }
  phenotypes <- OUT
  
  #probs      <- (m*phenotypes) + b    # directional selection; NOTE THAT THIS IS SOMETIMES INDENTICAL TO PHENPTYPES
  probs      <- -phenotypes^2+15       # stabilizing selection
  probs[which(probs <= 0)] <- 0.001    # negative probs converted to 0, care should be taken to not have negative probs in first place
  ptypes     <- cbind(1:nrow(pops), phenotypes)
  
  # selection strength
  #changing selection strength from directional selection (donor pops; old approach commented out)
  # new approach keeps survivors as proportion of survival probablity and shape of stabilizing selection curve
  #selection.strength <- (optima-mean(phenotypes))/optima    # strength of selection reduced as pop reaches optima
  #selection.strength <- abs(optima-mean(phenotypes))    # strength of selection reduced as pop reaches optima
  #if(selection.strength > 0.5) {selection.strength = 0.4} # despite above, cap selection at 0.5 at any point in time 
  #if(selection.strength <= 0) {selection.strength = 0.01} # always maintain slight selection?

  #keep <- nrow(pops) * (1-selection.strength) 
  keep <- length(which(probs >= 13)) # see plot of stabilizing selection to understand this
  
  keep <- sample(ptypes[, 1], keep, replace = FALSE, prob = probs)
  #hist(ptypes[keep, 2])
  
  pops <- pops[keep, ]
  return(pops)
}


