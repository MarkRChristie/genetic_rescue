Output <- function(pops, n, n.adult, output, n.kill) {
   larvae    <- pops[["larvae"]]    # larvae that are alread alive;
   juveniles <- pops[["juveniles"]] # juveniles that are alread alive;
   
   # demography
   # total number of larvae
   output.nlarvae <- output[["n.larvae"]]
   n.larvae.total <- cbind(n, length(larvae[, 1])) 
   output.nlarvae <- rbind(output.nlarvae, n.larvae.total)
   output[["n.larvae"]] <- output.nlarvae 

   # total number of juveniles
   output.njuveniles  <- output[["n.juveniles"]]
   n.juveniles.total  <- cbind(n, length(juveniles[, 1])) 
   output.njuveniles  <- rbind(output.njuveniles, n.juveniles.total)
   output[["n.juveniles"]] <- output.njuveniles 

   # total number of larvae per population
   output.nlarvs  <- output[["n.larvae.per.pop"]]
   n.larvs        <- cbind(n, data.frame(table(larvae[, 1])))
   output.nlarvs  <- rbind(output.nlarvs, n.larvs) 
   output[["n.larvae.per.pop"]] <- output.nlarvs

  # total number of adults
   output.nadults <- output[["n.adults"]]
   n.adults.total <- cbind(n, n.adult) 
   output.nadults <- rbind(output.nadults, n.adults.total)
   output[["n.adults"]] <- output.nadults 

   # age structure
   output.larvae.age.structure <- output[["larvae.age.structure"]]
   lages                       <- cbind(n, data.frame(table(larvae[, 2])))
   output.larvae.age.structure <- rbind(output.larvae.age.structure, lages)
   output[["larvae.age.structure"]] <- output.larvae.age.structure

   output.juv.age.structure    <- output[["juvenile.age.structure"]]
   jages                       <- cbind(n, data.frame(table(juveniles[, 2])))
   output.juv.age.structure <- rbind(output.juv.age.structure, jages)
   output[["juv.age.structure"]] <- output.juv.age.structure

   #resistance
   output.rjuvs   <- output[["resistant.juveniles"]]
   r.juvs         <- length(which(juveniles[, 6] == 1))
   n.rs.total     <- cbind(n, r.juvs) 
   output.rjuvs   <- rbind(output.rjuvs, n.rs.total)
   output[["resistant.juveniles"]] <- output.rjuvs 

   output.rlarvs  <- output[["resistant.larvae"]]
   r.larvs        <- length(which(larvae[, 6] == 1))
   n.rs.total     <- cbind(n, r.larvs) 
   output.rlarvs  <- rbind(output.rlarvs, n.rs.total)
   output[["resistant.larvae"]] <- output.rlarvs 

   #strength of selection
   output.nkilled <- output[["n.killed"]]
   if(length(n.kill) < 1) {n.kill <- 0}
   n.killed <- cbind(n, n.kill) 
   output.nkilled <- rbind(output.nkilled, n.killed)
   output[["n.killed"]] <- output.nkilled 
   
   return(output)
    #output <- cbind(i, out1, n.individs, n.species.t, n.tads.total, n.tads.inf, n.tads.uninf, n.adults.total, n.adults.inf, n.adults.uninf, n.inf, n.uninf)
    #write.table(output, paste(outdir, "Output.txt", sep = ""), col.names = FALSE, row.names = FALSE, sep="\t", append = TRUE)  

}
