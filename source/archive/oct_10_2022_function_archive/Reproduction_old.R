Reproduction <- function(n, pops, parameters, ncols, n.all.pairs) {
  adults  <- pops[pops >= 900000] # isolate all adult stage lamprey
  larvae  <- pops[pops < 800000] # larvae that are alread alive;
  juveniles <- pops[pops >= 800000 & pops < 900000]
  k.pops  <- parameters[["pops"]]
  n.resistant.adults <- parameters[["n.resistant.adults"]]
  year.added <- parameters[["resistant.year.added"]]
  gtype.key <- parameters[["gtype.key"]]
  resistance.cost <- parameters[["resistance.cost"]]
  egg.max <- parameters[["egg.max"]]
  egg.multiplier  <- parameters[["egg.multiplier"]]
  egg.addition    <- parameters[["egg.addition"]]  
   
  # add resistant individuals
  #if(n == resistant.year.added){
    #r.inds <- sample(1:length(adults), n.resistant.adults, replace = FALSE)
    #adults[r.inds] <- adults[r.inds] + 101
  #}

  n.adults <- length(adults)
  
  # randomly pair individuals (sex determined here)
  females <- adults[seq(1, length(adults), 2)]
  males <- adults[seq(2, length(adults), 2)]
  n.pairs <- min(c(length(females), length(males)))
  allpairs   <- cbind(males[sample(1:n.pairs, replace = FALSE)], females[sample(1:n.pairs, replace = FALSE)])
  
  #start new reproduction =================================================================================================#
  #temp.out   <- cbind(n, length(allpairs[, 1]))
  #write.table(temp.out, "pop_sizes.txt", col.names = FALSE, sep="\t", append = TRUE)  #MOVE THIS TO END!
  #year.data <- read.table("pop_sizes.txt", header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
  unaltered.all.pairs <- length(allpairs[, 1])
  year.data <- n.all.pairs
  
  #TEMPORARY FITNESS FUNCTION - move to PARAMETERS ==========================================================
  if (n > 1){
    
    #prior.year <- year.data[n-1, 3]
    prior.year <- year.data[1, 2]  
    F.n.adults <- 80
    F.K <- 500
    F.r <- 0.03
    #F.t <- 1:500 #use for plot
    F.t <- prior.year
    F.Y <- (F.K*F.n.adults)/(F.n.adults + ((F.K-F.n.adults) * exp(1)^(-F.r*F.t)))
    if(F.Y > length(allpairs[, 1])) {F.Y <- length(allpairs[, 1])}
    allpairs <- allpairs[sample(1:length(allpairs[, 1]), round(F.Y), replace = FALSE), ]
    plot(F.t, F.Y, ylim = c(0, max(F.Y) + 100))
  }
  
  n.all.pairs   <- cbind(n, length(allpairs[, 1]), unaltered.all.pairs)

  
  # allocate pairs based on larval abundances (would be interesting to turn this off and see how this affects evolution)
  #real.pops  <- data.frame(table(as.integer(substring(larvae, 1, 2))))
  #real.pops  <- cbind(real.pops, real.pops[, 2]/sum(real.pops[, 2]))
  #real.pops  <- cbind(real.pops, floor(real.pops[, 3] * n.pairs))
  #real.pops  <- cbind(real.pops, cumsum(real.pops[, 4]))

  #start <- c(1, real.pops[, 5] + 1)
  #start <- start[-length(start)]
  #stop  <- real.pops[, 5]

  POPS <- NULL
  #for(npop in 1:length(real.pops[, 1])){
    #pairs <- allpairs[seq(start[npop], stop[npop], 1), ,drop=FALSE]   
    
    npop  <- 1         #new to salmon1.0, previously ran through loop for each larval pop 
    pairs <- allpairs  #new to salmon1.0, previously ran through loop for each larval pop
  
    n.egg <- 100
    
    # original mechanism/stage of density depenence
    #n.egg <- as.integer(round(k.pops[npop, 2]/length(pairs[, 1]))) # caculate number of offspring per pair, not size based, see Ricker.R
    #n.egg <- (n.egg * egg.multiplier) + egg.addition
    #if(n.egg < 1) {n.egg <- 1}
    #if(n.egg > egg.max) {n.egg <- egg.max}
    
    # COULD ADD COST OF INFECTION/RESISTANCE RIGHT HERE (IF INFECTED, THEN LOWER REPRO OUTPUT)
    # Below creates offspring
    gtype1  <- as.integer(substring(pairs[, 1], 6, 6))
    gtype2  <- as.integer(substring(pairs[, 2], 6, 6))
    gtypes  <- paste(gtype1, gtype2, sep="")
    m1 <- match(gtypes, gtype.key[, 1])
    sample.frame <- gtype.key[m1, -1]  # create matrix to sample from with apply
    
    res <- which(as.numeric(gtypes) > 0) #identify resistant pairs
    
    if(length(res) < 1) {res <- 1} #offspring sampling if there are no resistant individualsm add cost to first individual (prevents crashing)
    sample.frame1 <- sample.frame[-res, , drop = FALSE] #no resistant pairs, use n.egg
    sample.frame2 <- sample.frame[res, , drop = FALSE] #resistant pairs, use n.egg * resistance.cost
    
    offs1 <- as.vector(apply(sample.frame1, 1, sample, n.egg, replace = TRUE))
    offs2 <- as.vector(apply(sample.frame2, 1, sample, n.egg*resistance.cost, replace = TRUE))
    OFFS <- as.numeric(c(offs1, offs2))
    
    

    pair.scept <- OFFS # add pair.scepts (i.e., resistnace)
    pair.scept[pair.scept > 0] <- 1

  POPS <- rbind(POPS, cbind(npop+10, OFFS, pair.scept)) #+10 needed to get correct pop
  #}
  


  popls      <- POPS[, 1]
  age        <- 1
  resistance <- POPS[, 3]  # may need to change this when introduce resitance
  gtypes     <- POPS[, 2]
  cost       <- 0
  
  new.larvae <- as.numeric(paste(popls, age, resistance, cost, gtypes, sep = ""))
  pops <- c(larvae, new.larvae, juveniles)  # add new.larvae 
  
  outpops <- list(pops, n.all.pairs)
  
  return(outpops)
}

