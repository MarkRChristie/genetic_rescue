JuvenileMortality <- function(pops, parameters) {
  juveniles <- pops[pops[, 4] == 1, ]
  adults    <- pops[pops[, 4] == 2, ]
  maximum.juvenile.age  <- parameters[["maximum.juvenile.age"]]  #there should not be any juveniles older than maximum.juvenile.age (no hard filter here though, only probablity, hard filter at ageing)
  juvenile.survival.var <- parameters[["juvenile.survival.var"]]
  k.juveniles           <- parameters[["k.juveniles"]]
  k.juv.year            <- parameters[["k.juv.year"]]
  
  juvkey <- 0:length(k.juv.year)
  
  JUVS <- NULL
  for(k in 1:length(k.juv.year)) {
    kyear <- k.juv.year[k]
    age   <- juvkey[k]
    juvs  <- juveniles[juveniles[, 3] == age, ]
    keep  <- kyear*k.juveniles  
    
#   #keep  <- round(rnorm(1, keep, juvenile.survival.var*keep))
    if(keep <= 0) {keep = length(juvs[, 1])} 
    
    if(keep < length(juvs[, 1])){ 
      #probs    <- (juveniles[, 3]/maximum.juvenile.age) # probablity favors the young (consider removing when adding species specific life histories)
      #probs[which(probs <= 0)] <- 0.001
      #pop1    <- sample(1:length(juveniles[, 1]), keep, replace=FALSE, prob = probs) # will cause a crash if not enough juveniles!
      pop1    <- sample(1:length(juvs[, 1]), keep, replace=FALSE) # will cause a crash if not enough juveniles!
      keepers <- juvs[pop1, ]
      JUVS    <- rbind(JUVS, keepers)
      } else {JUVS <- rbind(JUVS, juvs)}
    }
  

 # keep  <- round(rnorm(1, k.juveniles, juvenile.survival.var*k.juveniles))
# if(keep <= 0) {keep = length(juveniles[, 1])} 
    
# if(keep < length(juveniles[, 1])){ 
#    pop1    <- sample(1:length(juveniles[, 1]), keep, replace=FALSE) # will cause a crash if not enough juveniles!
#   keepers <- juveniles[pop1, ]
#   pops    <- rbind(adults, keepers)
# } else {pops <- rbind(adults, juveniles)}
  
 pops <- rbind(adults, JUVS)  
  
  return(pops)
}




