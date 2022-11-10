Replicates <- function(parameters, n.replicates) {
  # takes all *.set variables and creates the precise number of replicates for each variable
 
 k.adults.final    <- c(500, 1000)

 # example for when adding second variable
 # new variable     <- c(1, 2)
 # replicates <- expand.grid(resistance.cost, last.tfm.year, n.treated.streams)
 replicates <- as.data.frame(k.adults.final)
 #replicates <- replicates[rep(seq_len(nrow(replicates)), n.replicates), ]
 
 return(replicates)
}  
