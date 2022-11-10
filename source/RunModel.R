#  RunModel <- function(n.generations, i) {
  
  # pops columns are as follows:
  # 1 = ID (broodyear + unique number; e.g., 000_1 (individual 1 born in very first year), 000_987 (individual 987 born in first year), 101_19 (indivdiual 19 born in year 101))
  # 2 = sex (1=female, 2=male)
  # 3 = age
  # 4 = stage (driven by growth rate parameter and/or age)
  # 5 = WH (hatchery or wild origin, 1=wild, 2 = hatchery)
  # 6 = captive ancestry (amount of captive ancestry; average of parent ancestry where 1=hatchery and 0 =wild ; may want better equation)
  # 7 = growth rate parameter
  # 8 = dominance deviation
  
  # 7-10 = trait slots; all trait values are driven by genome (environment acts later)
  # 7 = growth rate
  # 8 = temperature tolerance (phenotype based on genotype)

  # 9+ = genetic loci 

  # model set up
  #Transformation_plots(parameters)
  #DensityDependence_plots(parameters)
  #Parameter_summary(parameters)
  rm(list = ls())
  setwd("C:/Users/fishf/Dropbox/manuscripts/rescue/salmon")
  library("hierfstat")
  library("inbreedR")
  library("purgeR")
  library("dplyr")
  library("beepr")

 
  base.directory <- getwd() 
  outdir         <- paste(base.directory,"/output/",sep="")  # directory to save model output  
  source(paste(base.directory, "/source/FunctionSourcer.R", sep = '')) #loads packages, sources functions, and sets source directory
  data.frame(unlist(parameters)) # show all parameters
  parameters[["k.adults.final"]] <- 100  # final population size
  parameters[["optima"]]         <- 0  # phenotypic optimum for local adaptation
  donor.pop <- read.table("../pops/out_300_0_pops.txt", header=TRUE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
  #initialize populations: create juveniles, adults
  juveniles<- Juveniles(parameters)
  adults   <- Adults(parameters) 
  pops     <- rbind(adults, juveniles)
  pops     <- Genotypes(pops, parameters)

  k.adults.final <- parameters[["k.adults.final"]]
  optima         <- parameters[["optima"]] 
  
  unlink("../output/pedigree.txt")  # make sure that pedigree file is deleted!

  for(n in 1:parameters[["n.years"]]){

    pops[, 3] <- pops[, 3] + 1 # increment ages by 1
    # if(length(pops) <= one.individual) {break}

    # change stage based on age
    ads <- which(pops[, 3] > parameters[["maximum.juvenile.age"]])  # default has been 2
    pops[ads, 4] <- 2
    
    pops     <- AdultMortality(pops, parameters)
    t.adults <- pops[pops$stage == 2, ]
    n.adult  <- nrow(t.adults) #required for output
    n.male   <- length(which(t.adults$sex == 2)) 
    n.female <- length(which(t.adults$sex == 1)) 

    # wild reproduction
    # write output when population functionally extinct (< 3 of 1 sex); then break
    if(n.male < 3 | n.female < 3) {
      write.table(pops, paste("../output/out", k.adults.final, optima, "pops.txt", sep="_"), col.names = TRUE, sep="\t", append = FALSE)
      write.table(output, paste("../output/out", k.adults.final, optima, "output.txt", sep="_"), col.names = TRUE, sep="\t", append = FALSE)
      print("extinct")
      break
      }                      # control structure for small # adults
    
    pops <- Reproduction(n, pops, parameters)

    # if(length(pops) <= one.individual) {break}   # break if only one adult (or less left)
    pops <- JuvenileMortality(pops, parameters)
    
    # SELECTION (used in genetic rescue recipient pops only to prevent drift) 
    if(n >= selection.start.year & n <= selection.end.year) {
      pops <- Selection(pops, parameters)
      }
    
    
    # POP SIZE CHANGING
    if(n >= ne.change.start & n <= ne.change.stop) {
      parameters[["k.adults"]] <- k.adults.final
      k.adults <- parameters[["k.adults"]]
      parameters[["k.juveniles"]] <- k.adults*k.juvenile.multiplier
      k.juveniles <- parameters[["k.juveniles"]]
      }
      
    # checks for additive variation and adds new (removed for now; constantly fights against stabilizing selection)
    #pops <- AdditiveVariation(pops, parameters)  # need to check that this works as expected
    
    if(any(n==parameters[["rescue.years"]])) {
      pops <- Rescue(pops, parameters)
    }
    
    # calculates output
    output <- Output(pops, n, n.adult, output) 
  
    # write output and pops files
    if(n == n.years) {
      write.table(pops, paste("../output/out", k.adults.final, optima, "pops.txt", sep="_"), col.names = TRUE, sep="\t", append = FALSE)
      write.table(output, paste("../output/out", k.adults.final, optima, "output.txt", sep="_"), col.names = TRUE, sep="\t", append = FALSE)
    }
      
    
  }

  pdf(file = "recipient_with_rescue.pdf", width = 6, height = 5) 
  #par(mfrow=c(3,1), oma = c(0, 2, 0, 2))
  plot(output[, 1], output[, 2], pch = 21, bg = "blue", cex = 2, main = "juveniles", ylim = c(0, max(output[, 2]) + 20)) # plot adults
  plot(output[, 1], output[, 4], pch = 21, bg = "blue", cex = 2, main = "adults", ylim = c(0, max(output[, 3]) + 200))   # plot adults
  plot(output[, 1], output[, 5], pch = 21, bg = "blue", cex = 2, main = "temperature tolerance") # plot temp tolerance
  plot(output[, 1], output[, 6], pch = 21, bg = "blue", cex = 2, main = "Ho", ylim = c(0, max(output[, 6]) + 0.5)) # plot juveniles
  plot(output[, 1], output[, 13], pch = 21, bg = "blue", cex = 2, main = "Ho2", ylim = c(0, max(output[, 13]) + 0.5)) # plot juveniles
  plot(output[, 1], output[, 7], pch = 21, bg = "blue", cex = 2, main = "Fis", ylim = c(min(output[, 7]) - 0.5, max(output[, 7]) + 0.5)) # plot juveniles
  plot(output[, 1], output[, 8], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 8]) + 5))   # plot juveniles
  plot(output[, 1], output[, 9], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 9]) + 5))   # plot juveniles
  plot(output[, 1], output[, 10], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 10]) + 5)) # plot juveniles
  plot(output[-c(1,2,3), 1], output[-c(1,2,3), 11], pch = 21, bg = "blue", cex = 2, main = "Ne1")                   # plot juveniles
  plot(output[, 1], output[, 12], pch = 21, bg = "blue", cex = 2, main = "inbreed")  
  plot(output[, 1], output[, 14], pch = 21, bg = "blue", cex = 2, main = "True F")#
  plot(output[-c(1:50), 14], output[-c(1:50), 3], pch = 21, bg = "green", cex = 2, main = "F effects", xlab="F", ylab="N adults") # F vs n adults
  plot(output[, 1], output[, 3], pch = 21, bg = "blue", cex = 2, main = "adults", ylim = c(0,60))   # plot adults

  # create plot of ages through time
  agevals <- output[, 15]
  agevals <- unlist(strsplit(agevals, "/")) 
  ages    <- output[, 16]
  ages    <- unlist(strsplit(ages, "/"))
  ages    <- cbind(1:length(agevals), agevals, ages)
  age     <- unique(ages[, 2])

  ayears  <- strsplit(output[, 15], "/") 
  alengths<- lengths(ayears) 
  ayears  <- rep(1:nrow(output), alengths)

  ages <- cbind(ayears, ages)

  #age=age[c(4, 5)] # just look at adults; toggle on and off
  
  colors <- c("orange", "dodgerblue", "black", "blue", "purple")
  plot(-10, -10, xlim=c(0, nrow(output)), ylim = c(0, max(as.numeric(ages[, 4]))), xlab="year", ylab="count")
  for(a in age){
    agen <- ages[which(ages[, 3] == a), ]
    lines(as.numeric(agen[, 1]), as.numeric(agen[, 4]), col=colors[as.numeric(a)+1], lwd=2)

  }  

  tail(output)
  dev.off()
  beep(3)

  #}
  