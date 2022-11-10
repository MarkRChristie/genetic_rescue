Infection3 <- function(hosts, days, threshold, plength, pwidth) {
  
  hosts <- cbind(hosts, 1:length(hosts[, 1])) # give each individual unique id in order to only have 1 inection per individual
  row   <- length(hosts[, 1]) * days
  OUT   <- matrix(0, row, ncol(hosts))

  start <- seq(1, length(OUT[, 1]), length(hosts[, 1]))
  stop <- start-1
  stop <- c(stop[-1], length(OUT[, 1]))

  x.positions <- seq(from = 0, to = plength, by = plength/10000) # determine resolution (pixel density) of pond
  y.positions <- seq(from = 0, to = pwidth, by = pwidth/10000) # determine resolution (pixel density) of pond
  for (d in 1:days){
    hosts[, 2]  <- sample(x.positions, length(hosts[, 2]), replace = TRUE)
    hosts[, 3]  <- sample(y.positions, length(hosts[, 3]), replace = TRUE)
    OUT[start[d]:stop[d], ] <- hosts
  }

  sensorx <- median(c(0, plength))
  sensory <- median(c(0, pwidth))
 
  x <- abs(sensorx - OUT[, 2])
  y <- abs(sensory - OUT[, 3])

  x1 <- which(x < threshold)
  y1 <- which(y < threshold)
  m1 <- match(y1, x1)
  contacts <- y1[(which(is.na(m1) == FALSE))] 
  contacts <- length(unique(OUT[contacts, 4]))
  hosts <- hosts[, -4] # get rid of temporary ids

  new.infected <- contacts*length(which(hosts[, 1] == 2))
  infects <- unique(sample(1:length(hosts[, 1]), new.infected, replace = TRUE))
  hosts[infects, 1] <- 2

  return(hosts)     
}
