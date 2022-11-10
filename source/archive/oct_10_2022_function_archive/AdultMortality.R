AdultMortality <- function(pops, parameters) {
  adults    <- pops[pops[, 4] == 2, ]
  others    <- pops[pops[, 4] != 2, ]
  adult.survival.var <- parameters[["adult.survival.var"]]
  k.adults           <- parameters[["k.adults"]]
  
  keep     <- round(rnorm(1, k.adults, adult.survival.var))
  
  if(keep < length(adults[, 1])){ 
    adults   <- adults[sample(1:length(adults[, 1]), keep, replace=FALSE), ] # will cause a crash if not enough juveniles!
  }
  
  pops <- rbind(others, adults)
  return(pops)
}



