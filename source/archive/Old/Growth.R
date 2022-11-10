#==================================================================================================#
# Script created by Mark Christie, contact at Redpath.Christie@gmail.com
# Script created in version R 3.3.1
# This script:  Generates model output for Lampre Resistance project
# Usage notes: Set all parameters below and then source this file
#==================================================================================================#
# Set working directory and output directory
# Directory where model.R and 'source' folder reside  
e <- exp(1)
L = 150     # maximum size
K = 0.003   # controls shape of the curve
K.sd <- 0.001
#K.sd <- 0.0001 
tzero = 5   # size at age 0
t= 50       # current time (in days) 
y <- L * (1-e^(-K * (t-tzero))) # Von Bert growth function
years <- 5


days     <- seq(from = 1, to = (365*years), by = 10)
k.values <- rnorm(100, K, K.sd)

#note that this is slow, but in model will only have to solve for a single uear (consider using apply)
OUT <- NULL
for(K in k.values){
  for(t in days){
    y   <- L * (1-e^(-K * (t-tzero)))
    out <- cbind(K, t, y)
    OUT <- rbind(OUT, out)
  }
}

plot(-10, -10, xlim = c(0, max(days)), ylim = c(0, L + 10), xlab = "Days", ylab = "Size (mm)")
for(n in unique(OUT[, 1])){
  dat <- OUT[OUT[, 1] == n, ] 
  lines(dat[, 2], dat[, 3], col = "blue")
}


