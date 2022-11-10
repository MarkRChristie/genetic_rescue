Juveniles <- function(parameters) {

  minimum.juvenile.age <- 0
  maximum.juvenile.age <- parameters[["maximum.juvenile.age"]]
  k.juveniles          <- parameters[["k.juveniles"]]
  pops                 <- parameters[["pops"]]
  #total.n              <- sum(pops[, 2])+1 # total number of needed larvae, needed here to not replicate ID
  
  
  ID         <- paste(0, "_", 1:k.juveniles, sep = "")     # birth year + unique number, at start of model all indivdiuals have birth.year 0 despite age
  age        <- rep(minimum.juvenile.age:maximum.juvenile.age, k.juveniles)
  age        <- sample(age, k.juveniles, replace = FALSE)  # age classes are roughly equal at the start of the simulation; could change this to decline with age to speed up "age structure" stabilization
  
  #size <- rnorm(1:length(age), parameters[["shape.parameter"]], parameters[["shape.var"]]) 
  stage  <- 1  
  sex    <- sample(rep(1:2, k.juveniles*10), k.juveniles, replace = TRUE)
  WH     <- 1 # all individuals are initially wild 
  gtypes <- 0
  extra  <- 0
  captive <- 0
  growth_rate  <- 0
  d.dev    <- 0
  
  population <- data.frame(ID, sex, age, stage, WH, captive, growth_rate, d.dev)
}




