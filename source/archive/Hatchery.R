Hatchery <- function(n, pops, parameters) {
  larvae    <- pops[pops[, 5] == 1, ] 
  juveniles <- pops[pops[, 5] == 2, ]
  adults    <- pops[pops[, 5] == 3, ]
  
  min.brood       <- parameters[["min.brood"]]
  k.pops          <- parameters[["pops"]]
  egg.max         <- parameters[["egg.max"]]
  egg.multiplier  <- parameters[["egg.multiplier"]]
  egg.addition    <- parameters[["egg.addition"]]  
  current.year    <- n
  
  
  #only take wild-origin adults 
  n.adults      <- length(adults[, 1])
  n.wild.adults <- length(which(adults$WH == 1))  #1=wild, 2=hatch 
  n.brood       <- round(n.wild.adults * percent.brood)
  if(n.brood > min.brood){
    wilds <- adults[adults$WH == 1, ]

    # randomly pair individuals 
    females <- wilds[wilds[, 6] == 1, ]
    males   <- wilds[wilds[, 6] == 2, ]
    n.pairs <- min(c(length(females[, 1]), length(males[, 1])))
    pairs   <- cbind(males[sample(1:n.pairs, replace = FALSE), ], females[sample(1:n.pairs, replace = FALSE), ])
    
    pairs <- pairs[sample(1:length(pairs[, 1]), round(n.brood/2), replace = FALSE), ]
    
    # write broodstock to file
    bout <- cbind(current.year, pairs[, 1], pairs[, 12])
    write.table(bout, paste(outdir, "broodstock.txt", sep = ""), sep =",", col.names = FALSE, row.names = FALSE, append = TRUE)
    
    n.egg <- as.integer(round(n.pairs*2)) # calculate number of offspring per pair
    n.egg <- 1000/(n.egg/2) # See DensityDepenence_plots for why this step is needed
    n.egg <- (n.egg * egg.multiplier) + egg.addition
    if(n.egg < 1) {n.egg <- 1}
    if(n.egg > egg.max) {n.egg <- egg.max}
    
    #add in Variance in RS (new)
    #mean(rnbinom(1000, 1, mu = 20)) # mu set equal to mean (here set equal to n.egg)
    #hist(rnbinom(1000, 1, mu = 20)) # mu set equal to mean (here set equal to n.egg)
    n.eggs <- rnbinom(length(pairs[, 1]), 1, mu = n.egg)
    none   <- which(n.eggs == 0) # ensure all pairs have at least one egg
    n.eggs[none] <- 1
      
    # Below creates offspring  
    POPS  <- NULL
    for(n in 1:length(pairs[, 1])){
      
      gtype1  <- pairs[n, 1:11]
      gtype2  <- pairs[n, 12:22]
      n.egg   <- n.eggs[n]
      #=======================================#
      # add in mendelian inheritance here!!!
      #=======================================#
      
      ID         <- paste(current.year, ".", n, ".", 1:n.egg, ".", "h", sep = "") 
      family     <- paste(current.year, "_", n, "_", gtype1[1], "-", gtype1[7], "_", gtype2[1], "-", gtype2[7], "_", "h", sep = "")  #cross_year _ pair.number _ parent_id1 - parent1HW _ parent_id2 - parent2HW _ birth environemnt (hw)
      #family     <- paste(current.year, ".", n, ".", gtype1[1], "-", gtype1[7], ".", gtype2[1], "-", gtype2[7], ".", "h", sep = "")  #cross_year + parent_id1 - parent1HW + parent_id2 - parent2HW
      birth.year <- current.year
      age        <- 0
      stage  <- 1  
      sex    <- sample(rep(1:2, n.egg*10), n.egg, replace = TRUE)
      WH     <- 2 # all individuals in this function are born in a hatchery 
      growth <- 0 # growth parameter (not inhereted in this model)
      gtypes <- 0
      extra  <- 0
      
      offspring <- data.frame(ID, family, birth.year, age, stage, sex, WH, growth, extra, extra, gtypes)
      POPS <- rbind(POPS, offspring) #+10 needed to get correct pop
    }
    
    new.larvae <- POPS
    # remove broodstock adults from population permenantley
    m1 <- match(pairs[, 1], adults[, 1])
    m2 <- match(pairs[, 12], adults[, 1])
    
    new.adults <- adults[-c(m1, m2), ] # remove broodstock adults from pops object
    
    
    pops <- rbind(larvae, new.larvae, juveniles, new.adults)  # new.adults = adults with no 
    return(pops)
  } else {pops[, 9] <- 1; return(pops)} #indicate in col 9 that there was no hatchery supplementation
}
