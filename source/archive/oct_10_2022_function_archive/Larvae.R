Larvae <- function(parameters) {

  pops    <- parameters[["pops"]]
  total.n <- sum(pops[, 2]) # total number of needed larvae

  ID         <- paste(0, ".",0, ".", 1:total.n, ".", "w", sep = "")     # birth year + family + unique number + hw, at start of model all indivdiuals have birth.year 0 despite age
  family     <- 0
  birth.year <- 0
  age        <- rep(0:maximum.larval.age, total.n)     # create age column
  age        <- sample(age, total.n, replace = FALSE)  # age classes are roughly equal at the start of the simulation; could change this to decline with age to speed up "age structure" stabilization
  
  #size <- rnorm(1:length(age), parameters[["shape.parameter"]], parameters[["shape.var"]]) 
  stage  <- 1  
  sex    <- sample(rep(1:2, total.n*10), total.n, replace = TRUE)
  WH     <- 1 # all individuals are initially wild 
  growth <- 0 # growth parameter (not inhereted in this model)
  gtypes <- 0
  extra  <- 0
  
  population <- data.frame(ID, family, birth.year, age, stage, sex, WH, growth, extra, extra, gtypes)
  #return(population)
}


