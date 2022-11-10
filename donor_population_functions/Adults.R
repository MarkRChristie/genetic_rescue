Adults <- function(parameters) {

  minimum.adult.age    <- parameters[["maximum.juvenile.age"]] + 1
  maximum.adult.age    <- parameters[["maximum.adult.age"]] + 1
  k.adults             <- parameters[["k.adults"]]
  k.juveniles          <- parameters[["k.juveniles"]]+1
  pops                 <- parameters[["pops"]]
  
  ID         <- paste(0, "_", k.juveniles:(k.juveniles+k.adults-1), sep = "")     # birth year + unique number, at start of model all indivdiuals have birth.year 0 despite age
  age        <- rep(minimum.adult.age:maximum.adult.age, k.adults)
  age        <- sample(age, k.adults, replace = FALSE)  # age classes are roughly equal at the start of the simulation; could change this to decline with age to speed up "age structure" stabilization
  
  #size <- rnorm(1:length(age), parameters[["shape.parameter"]], parameters[["shape.var"]]) 
  stage  <- 2  
  sex    <- sample(rep(1:2, k.adults*10), k.adults, replace = TRUE)
  WH     <- 1 # all individuals are initially wild 
  gtypes <- 0
  extra  <- 0
  captive <- 0
  growth_rate  <- 0
  d.dev    <- 0
  
  population <- data.frame(ID, sex, age, stage, WH, captive, growth_rate, d.dev)
}




