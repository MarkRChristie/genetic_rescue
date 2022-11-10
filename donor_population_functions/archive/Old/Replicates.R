Replicates <- function(proportion.metamorph, adult.survival.var, tadpole.survival.var, r, k.tads, k.adults, host.suscepts, range.suscepts, days, threshold, n.replicates) {
  # takes all *.set variables and creates the precise number of replicates for each variable
 
 samples <- paste(k.adults, k.tads)
 scepts  <- paste(host.suscepts-range.suscepts, host.suscepts+range.suscepts)
 
 replicates <- expand.grid(samples, scepts, adult.survival.var, tadpole.survival.var, proportion.metamorph, r, days, threshold)
 col0 <- unlist(strsplit(as.character(replicates[, 1]), " "))
 col1 <- as.numeric(col0[seq(1, length(col0), 2)])
 col2 <- as.numeric(col0[seq(2, length(col0), 2)])

 col0 <- unlist(strsplit(as.character(replicates[, 2]), " "))
 col3 <- as.numeric(col0[seq(1, length(col0), 2)])
 col4 <- as.numeric(col0[seq(2, length(col0), 2)])

 replicates <- cbind(col1, col2, col3, col4, replicates[, -c(1,2)])
 replicates <- replicates[rep(seq_len(nrow(replicates)), n.replicates), ]

 replicates[which(replicates[, 3] < 0), 3] <- 0 # new
 replicates[which(replicates[, 4] > 1), 4] <- 1 # new
 return(replicates)
}  