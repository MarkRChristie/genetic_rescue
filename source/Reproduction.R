Reproduction <- function(n, pops, parameters) {
  juveniles      <- pops[pops[, 4] == 1, ]
  adults         <- pops[pops[, 4] == 2, ]
  n.neutral.loci <- parameters[["n.neutral.loci"]]
  n.loci         <- parameters[["n.loci"]]
  
  k.pops          <- parameters[["pops"]]
  egg.max         <- parameters[["egg.max"]]
  egg.multiplier  <- parameters[["egg.multiplier"]]
  egg.addition    <- parameters[["egg.addition"]]  
  dom             <- parameters[["dominance.effect"]]  
  
  #inbreeding.fitness.cost <- parameters[["inbreeding.fitness.cost"]]
  #if(n == 1){inbreed_F = 0} else {inbreed_F <- output[nrow(output), 14]} # take most recent estimate of F
  
  # KEY BASED ON TEST (PULL TEST LINE UP HERE FOR FUTURE UNDERSTANDING)
  #  gtype <- cbind(sum(asp > 0), sum(asp < 0)) # key to understanding key
  # currently assumes same dominance effect for each locus
  # see KEY.jpg (source folder) for explanation of how key works
  key1 <- c(0, 0, 2, 0, 1, 1)
  key2 <- c(2, 0, 0, 1, 1, 0)
  key3 <- c(-dom,  -dom, dom,  0.00, 0.00, 0.00) 
  key4 <- c(-dom,  0.00, dom,  0.00, 0.00, 0.00) 
  key5 <- c(-dom,  0.00, dom,  -dom, 0.00, dom) 
  key6 <- c(-dom,   dom, dom,  -dom, 0.00, dom) 
  key  <- cbind(key1, key2, key3, key4, key5, key6)


  current.year    <- n
  
  #n.adults <- length(adults[, 1])
  
  # randomly pair individuals (sex determined here)
  females <- adults[adults[, 2] == 1, ]
  males   <- adults[adults[, 2] == 2, ]
  n.pairs <- min(c(length(females[, 1]), length(males[, 1])))
  pairs   <- cbind(males[sample(1:n.pairs, replace = FALSE), ], females[sample(1:n.pairs, replace = FALSE), ])

  n.egg <- as.integer(round(n.pairs*2)) # calculate number of offspring per pair
  n.egg <- 1000/(n.egg/2) # See DensityDepenence_plots for why this step is needed
  n.egg <- (n.egg * egg.multiplier) + egg.addition
  if(n.egg < 1) {n.egg <- 1}
  if(n.egg > egg.max) {n.egg <- egg.max}
  n.egg <- round(n.egg)
  
 #if(inbreeding.fitness.cost == 1){
  #  n.egg <- n.egg*(1-(inbreed_F*8)) # INBREEDING PENALTY!; currently linear
  #}
  
  # penalize reproduction

   # set up all offspring matrix for neutral reproduction
   #ALLOFFSPRING      <- matrix(nrow = n.pairs*n.egg, ncol = 1 + (n.neutral.loci * 2)) #IF VARIANCE EXCEEDS 0.25 EXTRA OFFSPRING, WILL GET ERROR
   #ALLOFFSPRING[, 1] <- -9
  
  
  #add in Variance in RS (new)
  #mean(rnbinom(1000, 1, mu = 20)) # mu set equal to mean (here set equal to n.egg)
  #hist(rnbinom(1000, 1, mu = 20)) # mu set equal to mean (here set equal to n.egg)
  # currently off
  #n.eggs <- rnbinom(length(pairs[, 1]), 1, mu = n.egg)
  #none   <- which(n.eggs == 0) # ensure all pairs have at least one egg
  #n.eggs[none] <- 1
  
  # Below creates offspring  
  POPS  <- NULL
  for(n in 1:length(pairs[, 1])){
    dims    <- ncol(adults)-(n.neutral.loci*2) # seprate out adaptive loci first
    gtype1  <- pairs[n, 1:dims]
    gtype2  <- pairs[n, ((ncol(pops)+1):(ncol(pops)+dims))]
    #n.egg   <- n.eggs[n]
    #=======================================#
    # add in mendelian inheritance here!!!
    #=======================================#

    ID         <- paste(current.year, "_", n, "_", 1:n.egg, "_", "w", sep = "") # could add d to donor indivdiuals
    family     <- paste(pairs[n, 1], pairs[n, 229], sep = "/")
    #family     <- paste(current.year, "_", n, "_", gtype1[1], "-", gtype1[7], "_", gtype2[1], "-", gtype2[7], "_", "w", sep = "")  #cross_year _ pair.number _ parent_id1 - parent1HW _ parent_id2 - parent2HW _ birthenvironment(HW)
    #birth.year <- current.year
    age    <- 0
    stage  <- 1  
    sex    <- sample(rep(1:2, n.egg*10), n.egg, replace = TRUE)
    WH     <- 1 # all individuals in this function spawn in the wild 
    #growth_rate <- 0 # growth parameter (not inherited in this model)
    #gtypes <- 0
    #extra  <- 0
    
    #begin reproduction
    #captive     <- mean(gtype1$captive, gtype2$captive)
    growth_rate <- mean(gtype1$growth_rate, gtype2$growth_rate)
    d.dev       <- gtype1$d.dev   # currently inherited from mom (consider moving to parameters)
    
    locus.start <- ncol(pops)-(n.neutral.loci*2)-n.loci+1
    loci <- (locus.start:(locus.start+(n.loci-1))) 
    LOCI <- NULL
    for(l in loci){
    
    asp   <- c(gtype1[, l], gtype2[, l]) # what is gtype of parents
    gtype <- cbind(sum(asp > 0), sum(asp < 0)) # 
    m1    <- match(key[, 1], gtype[1])
    m2    <- match(key[, 2], gtype[2])
    
    offs  <- key[which((m1+m2) == 2), 3:6] # may want a check/warning built in here
    offs2 <- sample(offs, n.egg, replace = TRUE)  
    LOCI  <- cbind(LOCI, offs2)
    }
    
    # start of neutral reproduction============================#
    dims    <- ncol(adults)-(n.neutral.loci*2) # seprate out adaptive loci first
    fg      <- pairs[n, (dims+1):(dims+(n.neutral.loci*2))]
    mg      <- pairs[n, (ncol(pops)+dims+1):ncol(pairs)] 
          
        
    # prep offspring genotype matrix
    offspringG = matrix(nrow=n.egg, ncol=(n.neutral.loci*2))
    
    # allele 1 positions
    positions = seq(1, (n.neutral.loci*2), 2)
    
    # randomly sample either position 1 or 2 (add 0 or 1) to starting position
    fallele  <- positions + sample(0:1, n.neutral.loci * n.egg, replace = TRUE)
    fallele2 <- fg[fallele]
    fallele3 <- matrix(fallele2, nrow = n.egg, ncol = n.neutral.loci, byrow = TRUE)
    
    mallele  <- positions + sample(0:1, n.neutral.loci * n.egg, replace = TRUE)
    mallele2 <- mg[mallele]
    mallele3 <- matrix(mallele2, nrow = n.egg, ncol = n.neutral.loci, byrow = TRUE)
    
    offspringG[, positions]     <- as.numeric(fallele3)
    offspringG[, positions + 1] <- as.numeric(mallele3)
   
    # end of neutral reproduction ==============================#

    captive   <- family  # family ID written to captive column because it was open/unused!
    offspring <- data.frame(ID, sex, age, stage, WH, captive, growth_rate, d.dev, LOCI, offspringG)
    POPS      <- rbind(POPS, offspring) #+10 needed to get correct pop
  }
  
  #POPS
  new.juveniles <- POPS
  colnames(new.juveniles) <- colnames(adults)
  pops <- rbind(adults, juveniles, new.juveniles)  # no adults; die after spawning
  return(pops)
}

