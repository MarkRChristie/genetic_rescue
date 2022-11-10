DensityDependence_plots <- function(parameters) {
  #relationship between number of spawners and eggs
  egg.multiplier <- parameters[["egg.multiplier"]]
  egg.addition   <- parameters[["egg.addition"]]
  egg.max        <- parameters[["egg.max"]]
  
  n <- 1:1000
  n.egg <- 1000/(n/2)
  
  n.eggs <- (n.egg * egg.multiplier) + egg.addition
  plot(n, n.eggs, ylim = c(0, egg.max + 20), main = "black:current; blue:0.25_and_5x egg multiplier")
  text(800, egg.max, paste("egg.max =", egg.max))
  
  n.eggs <- (n.egg * egg.multiplier * 5) + egg.addition
  points(n, n.eggs, ylim = c(0, 50), col = "blue")
  
  n.eggs <- (n.egg * egg.multiplier * 0.25) + egg.addition
  points(n, n.eggs, ylim = c(0, 50), col = "blue")


}



