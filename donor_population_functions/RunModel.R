RunModel <- function(n.generations, i) {
  
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
  
  #setwd("C:/Users/fishf/Dropbox/manuscripts/rescue/salmon")
  #library("hierfstat")
  #base.directory <- getwd()
  #outdir         <- paste(base.directory,"/output/",sep="")  # directory to save model output  
  #source(paste(base.directory, "/source/FunctionSourcer.R", sep = '')) #loads packages, sources functions, and sets source directory
  #data.frame(unlist(parameters)) # show all parameters
  #parameters[["k.adults.final"]] <- 500  # final population size
  #parameters[["optima"]]         <- 2  # phenotypic optimum for local adapation
    
  #initialize populations: create juveniles, adults
  juveniles<- Juveniles(parameters)
  adults   <- Adults(parameters) 
  pops     <- rbind(adults, juveniles)
  pops     <- Genotypes(pops, parameters)

  k.adults.final <- parameters[["k.adults.final"]]
  optima         <- parameters[["optima"]] 
# NEED TO ADD
# AFTER SELECTION = RUN CHECK FOR FIXED LOCI, ADD EFFECT TO DOMINANCE DEVIATION, REMOVE LOCUS, ADD NEW LOCUS
# NEED TO CHANGE SELECTION STRENGTH AS GET CLOSER TO OPTIMA
    
  
  for(n in 1:parameters[["n.years"]]){
    pops[, 3] <- pops[, 3] + 1 # increment ages by 1
    #if(length(pops) <= one.individual) {break}
    #pops     <- Transformation(pops, parameters)
    
    # change stage based on age
    ads <- which(pops[, 3] > 2)
    pops[ads, 4] <- 2
    
    pops     <- AdultMortality(pops, parameters)
    n.adult  <- length(which(pops$stage == 2)) #required for output
    #print(n.adult)
     
    
    # wild reproduction
    pops <- Reproduction(n, pops, parameters)

    
     #if(length(pops) <= one.individual) {break}   # break if only one adult (or less left)
    pops <- JuvenileMortality(pops, parameters)
    
    #SELECTION 
    if(n >= selection.start.year & n <= selection.end.year){
      pops <- Selection(pops, parameters)}
    
    
    #POP SIZE CHANGING
    if(n >= ne.change.start & n <= ne.change.stop){
      parameters[["k.adults"]] <- k.adults.final
      k.adults <- parameters[["k.adults"]]
      parameters[["k.juveniles"]] <- k.adults*k.juvenile.multiplier
      k.juveniles <- parameters[["k.juveniles"]]

      }
      
    # checks for additive variation and adds new
    pops <- AdditiveVariation(pops, parameters)  # need to check that this works as expected
    
    # calculates output
    output <- Output(pops, n, n.adult, output) 
    
    # write output and pops files
    if(n == n.years) {
      write.table(pops, paste("../pops/out", k.adults.final, optima, "pops.txt", sep="_"), col.names = TRUE, sep="\t", append = FALSE)
      write.table(output, paste("../pops/out", k.adults.final, optima, "output.txt", sep="_"), col.names = TRUE, sep="\t", append = FALSE)
    }
      
    
  }

  plot(output[, 1], output[, 2], pch = 21, bg = "blue", cex = 2, main = "juveniles", ylim = c(0, max(output[, 2]) + 20)) # plot adults
  plot(output[, 1], output[, 3], pch = 21, bg = "blue", cex = 2, main = "adults", ylim = c(0, max(output[, 3]) + 200)) #plot juveniles
  plot(output[, 1], output[, 5], pch = 21, bg = "blue", cex = 2, main = "temperature tolerance", ylim = c(0, max(output[, 5]) + 0.5)) #plot juveniles
  plot(output[, 1], output[, 6], pch = 21, bg = "blue", cex = 2, main = "Ho", ylim = c(0, max(output[, 6]) + 0.5)) #plot juveniles
  plot(output[, 1], output[, 7], pch = 21, bg = "blue", cex = 2, main = "Fis", ylim = c(0, max(output[, 7]) + 0.5)) #plot juveniles
  plot(output[, 1], output[, 8], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 8]) + 5)) #plot juveniles
  plot(output[, 1], output[, 9], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 9]) + 5)) #plot juveniles
  plot(output[, 1], output[, 10], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 10]) + 5)) #plot juveniles
  plot(output[-c(1,2,3), 1], output[-c(1,2,3), 11], pch = 21, bg = "blue", cex = 2, main = "Ne1") #plot juveniles

}