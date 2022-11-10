AdultMortality <- function(pops, parameters) {
  adults    <- pops[pops[, 4] == 2, ]
  others    <- pops[pops[, 4] != 2, ]
  adult.survival.var      <- parameters[["adult.survival.var"]]
  k.adults                <- parameters[["k.adults"]]
  max.adult.age           <- parameters[["maximum.adult.age"]]
  inbreeding.fitness.cost <- parameters[["inbreeding.fitness.cost"]]
  if(n == 1){inbreed_F = 0} else {inbreed_F <- output[nrow(output), 14]} # no value in gen 1
  
  adults <- adults[which(adults[, 3] <= max.adult.age), ]  # maximum age is set to 6! Hard cutoff!
  
  keep     <- round(rnorm(1, k.adults, adult.survival.var*k.adults))
  if(keep <= 0) {keep = length(adults[, 1])} 
  
  if(inbreeding.fitness.cost == 1){
    keep <- keep*(1-(inbreed_F*7)) # INBREEDING PENALTY!; *8 is a good, strong default; currently linear
  }
  
  
  if(keep < length(adults[, 1])){ 
   # probs    <- 1-(adults[, 3]/max(adults[, 3])) # probablity favors the young (consider removing when adding species specific life histories)
   # probs[which(probs <= 0)] <- 0.001
   # adults   <- adults[sample(1:length(adults[, 1]), keep, replace=FALSE, prob = probs), ] # will cause a crash if not enough juveniles!
   adults   <- adults[sample(1:length(adults[, 1]), keep, replace=FALSE), ] # will cause a crash if not enough juveniles!
  }
  
  pops <- rbind(others, adults)
  return(pops)
}



