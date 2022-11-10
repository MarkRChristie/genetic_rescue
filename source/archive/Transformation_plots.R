Transformation_plots <- function(parameters) {
  #larval transformations (to juvenile stage)
  transform.threshold <- parameters[["transform.threshold"]]
  L <- parameters[["maximum.larval.size"]]
  e <- parameters[["e"]]
  K <- rnorm(1:parameters[["pops"]][2], parameters[["shape.parameter"]], parameters[["shape.var"]])
  
  tzero <- parameters[["tzero"]]
  
  #plot
  K <- sort(K)
  plot(-10, -100, xlim = c(1,2000), ylim = c(0,200), xlab = "time", ylab = "size", main = "larvae to juvenile")
  larv.sizes.plot1 <-  L * (1-e^(-range(K)[1] * ((1:2000)-tzero)))  # minimum
  larv.sizes.plot2 <-  L * (1-e^(-range(K)[2] * ((1:2000)-tzero)))  # maximum
  larv.sizes.plot3 <-  L * (1-e^(-median(K) * ((1:2000)-tzero)))    # median
  larv.sizes.plot4 <-  L * (1-e^(-(K[length(K)*0.9]) * ((1:2000)-tzero)))    # top 90%
  larv.sizes.plot5 <-  L * (1-e^(-(K[length(K)*0.1]) * ((1:2000)-tzero)))    # top 90%
  
  
  points(1:2000, larv.sizes.plot1)
  points(1:2000, larv.sizes.plot2)
  points(1:2000, larv.sizes.plot3)
  points(1:2000, larv.sizes.plot4, col = "blue")
  points(1:2000, larv.sizes.plot5, col = "blue")
  abline(h = transform.threshold)
  points(c((365*1), (365*2), (365*3), (365*4), (365*5), (365*6)), rep(transform.threshold, 6), pch = 19, col = c("blue", "purple", "green", "orange", "black", "red"), cex = 2)
  
  
  #juvenile transformations (to adult stage)  - everything should transform after a single year as a juvenile!
  adult.threshold <- parameters[["adult.threshold"]]
  L <- parameters[["maximum.adult.size"]]
  e <- parameters[["e"]]
  K <- rnorm(1:parameters[["k.juveniles"]], parameters[["shape.parameter"]], parameters[["shape.var"]])
  
  tzero <- parameters[["tzero"]]
  
  K <- sort(K)
  plot(-10, -100, xlim = c(1,2000), ylim = c(0,800), xlab = "time", ylab = "size", main = "juvenile to adult")
  larv.sizes.plot1 <-  L * (1-e^(-range(K)[1] * ((1:2000)-tzero)))  # minimum
  larv.sizes.plot2 <-  L * (1-e^(-range(K)[2] * ((1:2000)-tzero)))  # maximum
  larv.sizes.plot3 <-  L * (1-e^(-median(K) * ((1:2000)-tzero)))    # median
  larv.sizes.plot4 <-  L * (1-e^(-(K[length(K)*0.9]) * ((1:2000)-tzero)))    # top 90%
  larv.sizes.plot5 <-  L * (1-e^(-(K[length(K)*0.1]) * ((1:2000)-tzero)))    # top 90%
  
  
  points(1:2000, larv.sizes.plot1)
  points(1:2000, larv.sizes.plot2)
  points(1:2000, larv.sizes.plot3)
  points(1:2000, larv.sizes.plot4, col = "blue")
  points(1:2000, larv.sizes.plot5, col = "blue")
  abline(h = adult.threshold)
  points(c((365*1), (365*2), (365*3), (365*4), (365*5), (365*6)), rep(adult.threshold, 6), pch = 19, col = c("blue", "purple", "green", "orange", "black", "red"), cex = 2)
  
  
  
}



