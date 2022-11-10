LarvalMortality <- function(pops, parameters) {
  larvae    <- pops[pops[, 5] == 1, ] 
  juveniles <- pops[pops[, 5] == 2, ]
  adults    <- pops[pops[, 5] == 3, ]
  
  k.larvae  <- parameters[["pops"]]
  larval.survival.var <- parameters[["larval.survival.var"]]
  maximum.larval.age  <- parameters[["maximum.larval.age"]]

  keep <- round(rnorm(1, k.larvae[, 2], larval.survival.var)) # really how many to keep

  if (keep < length(larvae[, 1])){
      pop1     <- sample(1:length(larvae[, 1]), keep, replace = FALSE)
      keepers <- larvae[pop1, ]
      pops <- rbind(keepers, juveniles, adults) #shouls really be no adults, but kept in for now anyway for future expansions
  } else {pops <- rbind(larvae, juveniles, adults)}
  return(pops)
}


