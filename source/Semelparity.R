Semelparity <- function(pops, parameters) {
  juveniles      <- pops[pops[, 4] == 1, ]
  adults         <- pops[pops[, 4] == 2, ]
  semelparity    <- parameters[["semelparity"]]

  if(semelparity == 1) { 
    pops <- juveniles  # no adults added; adults die after spawning
  }  

  return(pops)
}



