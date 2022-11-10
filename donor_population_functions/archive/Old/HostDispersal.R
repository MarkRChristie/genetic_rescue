HostDispersal <- function(hosts, plength, pwidth) {
  x.positions <- seq(from = 0, to = plength, by = plength/10000) # determine resolution (pixel density) of pond
  y.positions <- seq(from = 0, to = pwidth, by = pwidth/10000) # determine resolution (pixel density) of pond
  hosts[, 6]  <- sample(x.positions, length(hosts[, 6]), replace = TRUE)
  hosts[, 7]  <- sample(y.positions, length(hosts[, 7]), replace = TRUE)
  return(hosts)
}



