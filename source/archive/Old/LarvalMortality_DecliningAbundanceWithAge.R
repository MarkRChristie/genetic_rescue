LarvalMortality <- function(pops, parameters) {
  larvae    <- pops[["larvae"]] # larvae that are alread alive;
  k.larvae  <- parameters[["pops"]]
  larval.survival.var <- parameters[["larval.survival.var"]]

  n.larvae <- table(larvae[, 1])
  n.pops   <- length(k.larvae[, 1])

  larva.mortality <- round(rnorm(n.pops, k.larvae[, 2], larval.survival.var))

  OUT <- NULL
  for(n in 1:n.pops){
    pop1  <- larvae[larvae[, 1] == n, ]
    keep  <- larva.mortality[n]
 
    probs <- pop1[, 2]
    probs[which(probs == 1)] <- 0.1 # was 0.001 
    probs[which(probs > 1)] <- 1 

    if(keep < length(probs)){
      keepers <- sample(1:length(probs), keep, replace = FALSE, prob = probs)
      pop2    <- pop1[keepers, ]
    } else {pop2 <- pop1}

    OUT <- rbind(OUT, pop2)
  }  
  
  larvae <- OUT
  pops[["larvae"]] <- larvae
  return(pops)
}



