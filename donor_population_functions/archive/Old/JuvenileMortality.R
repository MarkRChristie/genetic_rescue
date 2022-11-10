JuvenileMortality <- function(pops, parameters) {
  juveniles <- pops[pops[, 4] == 1, ]
  adults    <- pops[pops[, 4] == 2, ]
  juvenile.survival.var <- parameters[["juvenile.survival.var"]]
  k.juveniles           <- parameters[["k.juveniles"]]

  keep  <- round(rnorm(1, k.juveniles, juvenile.survival.var))
  
  if(keep < length(juveniles[, 1])){ 
    pop1    <- sample(1:length(juveniles[, 1]), keep, replace=FALSE) # will cause a crash if not enough juveniles!
    keepers <- juveniles[pop1, ]
    pops <- rbind(adults, keepers)
    } else {pops <- rbind(adults, juveniles)}
  return(pops)
}


