AdultEmmigration <- function(hosts) {
  adults <- hosts[hosts[, 4] == 2, ]
  hosts  <- hosts[hosts[, 4] == 1, ]
  new    <- list(hosts, adults)
  return(new)
}



