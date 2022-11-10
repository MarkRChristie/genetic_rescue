Rescue <- function(pops, parameters) {
  rescue.number <- parameters[["rescue.number"]]
  rescue.age    <- parameters[["rescue.age"]]
  
  donor.inds    <- donor.pop[donor.pop[, 3] == rescue.age, ]
  donor.inds    <- donor.pop[sample(seq(from = 1, to = nrow(donor.inds), by = 1), rescue.number, replace = FALSE), ] # cannot sample more than in population
  
  colnames(donor.inds) <- colnames(pops) # make colnames match
  pops <- rbind(pops, donor.inds)
  
  return(pops)
}