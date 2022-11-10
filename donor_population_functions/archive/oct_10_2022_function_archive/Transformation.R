Transformation <- function(pops, parameters) {
  larvae    <- pops[pops[, 5] == 1, ] # larvae that are alread alive
  juveniles <- pops[pops[, 5] == 2, ]
  
  #larval transformations (to juvenile stage)
  transform.threshold <- parameters[["transform.threshold"]]
  L <- parameters[["maximum.larval.size"]]
  e <- parameters[["e"]]
  K <- rnorm(1:length(larvae[, 1]), parameters[["shape.parameter"]], parameters[["shape.var"]])
  t <- larvae$age * 365 #extract age in years and multiply by days

  tzero <- parameters[["tzero"]]
  larv.sizes <-  L * (1-e^(-K * (t-tzero)))
  new.juvs <- which(larv.sizes >= transform.threshold)
  larvae[new.juvs, 5] <- 2
  
  
  #juvenile transformations (to adult stage)  - 
  adult.threshold <- parameters[["adult.threshold"]]
  L <- parameters[["maximum.adult.size"]]
  e <- parameters[["e"]]
  t <- juveniles$age * 365 #extract age in years and multiply by days
  K <- rnorm(1:length(juveniles[, 1]), parameters[["shape.parameter"]], parameters[["shape.var"]])
  
  tzero <- parameters[["tzero"]]
  juv.sizes <-  L * (1-e^(-K * (t-tzero)))
  new.adults <- which(juv.sizes >= adult.threshold)
  juveniles[new.adults, 5] <- 3
  
  pops <- rbind(larvae, juveniles)
  return(pops)
}



