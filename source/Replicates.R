Replicates <- function(parameters, n.replicates) {
  # takes all *.set variables and creates the precise number of replicates for each variable
 
 # parameters for donor populations
 k.adults.final    <- c(100, 300)
 optima <- c(2, 0)

 

 replicates <- expand.grid(k.adults.final, optima)
 #replicates <- as.data.frame(k.adults.final)
 replicates <- replicates[rep(seq_len(nrow(replicates)), n.replicates), ]
 
 return(replicates)
}  
