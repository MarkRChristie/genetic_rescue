AdultMortality <- function(pops, parameters) {
  adults    <- pops[pops[, 4] == 2, ]
  others    <- pops[pops[, 4] != 2, ]
  adult.survival.var <- parameters[["adult.survival.var"]]
  k.adults           <- parameters[["k.adults"]]
  
  keep     <- round(rnorm(1, k.adults, adult.survival.var*k.adults))
  if(keep <= 0) {keep = length(adults[, 1])} 
  
  
  if(keep < length(adults[, 1])){ 
    probs    <- 1-(adults[, 3]/max(adults[, 3])) # probablity favors the young (consider removing when adding species specific life histories)
    probs[which(probs <= 0)] <- 0.001
    adults   <- adults[sample(1:length(adults[, 1]), keep, replace=FALSE, prob = probs), ] # will cause a crash if not enough juveniles!
  }
  
  pops <- rbind(others, adults)
  return(pops)
}



