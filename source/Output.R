Output <- function(pops, n, n.adult, output) {
  if(n == 1) {output <- NULL} # initialize output object as NULL
  juveniles <- pops[pops[, 4] == 1, ]
  adults    <- pops[pops[, 4] == 2, ]
  n.loci    <- parameters[["n.loci"]]
  n.neutral.loci    <- parameters[["n.neutral.loci"]]
   
  n.juveniles <- length(juveniles[, 1])
  nadult.1    <- n.adult
  nadult.2    <- nrow(adults) # adults right now in the population (after mortality)
  
  dims        <- ncol(pops)-(n.neutral.loci*2)-n.loci # seprate out adaptive loci first
  phenotypes  <- pops[, (dims+1):(dims+n.loci)]
  phenotypes  <- mean(rowSums(phenotypes))  + pops[1, 8] # takes first d.dev value
  gtypes      <- pops[, (dims+n.loci+1):ncol(pops)]
  
  # format data for hierfstat to calculate Fis
  FORMAT <- NULL
  for(c in seq(from = 1, to = ncol(gtypes), by = 2)){
    dat <- gtypes[, c(c, c+1)]
    dat <- as.numeric(paste(dat[, 1], dat[, 2], sep = ""))
    FORMAT <- cbind(FORMAT, dat)
  }

  gtypes <- cbind(99, FORMAT) # add pop "99" for pop label for hierfstat
  gstats <- basic.stats(data.frame(gtypes), diploid=TRUE, digits=4) # must be a data frame
  fis    <- gstats$overall[9]
  ho    <-  gstats$overall[1]

  # format data for inbreedR
  gtypes2  <- pops[, (dims+n.loci+1):ncol(pops)]
  gtypes2  <- convert_raw(gtypes2) 
  g2       <- g2_microsats(gtypes2, nperm = 0, nboot = 0, CI = 0.95)
  inbreed  <- g2[[2]] # estimate of inbreeding from neutral markers
  het2      <- mean(MLH(gtypes2))
  #g2 <- g2_snps(gtypes2, nperm = 100, nboot = 100, CI = 0.95)  # use this with larger numbers of loci!
  
  # calculate NE
  ne1 <- 1/(sum(1/output[, 2])/nrow(output))  # harmonic mean of adults
  ne2 <- 1/(sum(1/output[, 3])/nrow(output))  # harmonic mean of adults.2  
  ne3 <- 1/(sum(1/(output[, 2] + output[, 3]))/nrow(output)) # harmonic mean of adults + juveniles
  
  adult.ids <- adults[, 1]
  families  <- unlist(strsplit(adult.ids, "_"))
  f.years   <- families[seq(1, length(families), 4)] # query here for mean parent age
  f.fam     <- families[seq(2, length(families), 4)] # query here for mean parent age
  family    <- paste(f.years, f.fam, sep="_") 
  #hist(table(family))  family sizes
  f.sizes <- data.frame(table(family))
  k  <- mean(f.sizes[, 2])
  vk <- var(f.sizes[, 2])
  N  <- nrow(f.sizes)
  ne4 <- ((N*k)-1)/(k-1+(vk/k))
  
  # return pedigree starting right 
  if(n >= (parameters[["ne.change.start"]])-10){
    ped <- cbind(n, adult.ids, adults[, 6])
    write.table(ped, "../output/pedigree.txt", col.names = FALSE, sep="\t", append = TRUE)
  }
  
  # start calculating F, 5 years after start of ne change
  if(n >= (parameters[["ne.change.start"]] + 5)){ 
    F_inbreeding <- Inbreeding_estimation_pedigree(parameters, n)
  }  else {F_inbreeding <- 0}

  # report ages
  ages    <- as.character(table(pops[, 3]))        # counts of the below age categories
  ages    <- paste(ages,sep="", collapse="/")
  agevals <- names(table(pops[, 3]))               # actual age categories
  agevals <- paste(agevals,sep="", collapse="/")
  

  
  if(n==1) {ne1=ne2=ne3=ne4=0} # nes for first generation for formatting
  #output.new <- data.frame(cbind(n, n.juveniles, nadult.1, nadult.2, phenotypes, ho, fis, ne1, ne2, ne3, ne4, inbreed, het2, F_inbreeding, agevals, ages))
  output.new <- data.frame(n, n.juveniles, nadult.1, nadult.2, phenotypes, ho, fis, ne1, ne2, ne3, ne4, inbreed, het2, F_inbreeding, agevals, ages)
  output     <- rbind(output, output.new)
  return(output)
}
