Inbreeding_estimation_pedigree <- function(parameters, n) {
  n.years <- n
  
  ped <- read.table("../output/pedigree.txt", header=FALSE, sep="\t", na.strings="?", dec=".", strip.white=TRUE)
  ped <- ped[which(ped[, 2] >= (n.years-20)), ] # calculate F for last 20 years only!!!
   
  #head(ped)
  #tail(ped)
  
  
  # formatting of pedigree
  ids        <- unlist(strsplit(ped[, 4], "/"))
  dams       <- ids[seq(from = 1, to = length(ids), by = 2)]
  sires      <- ids[seq(from = 2, to = length(ids), by = 2)]
  offspring  <- ped[, 3]
  
  inbreed.ped <- as.data.frame(cbind(offspring, dams, sires))
  
  # sort so that dams and sires come before offspring
  #inbreed.ped2 <- cbind(1:length(inbreed.ped2[, 1]), inbreed.ped2)
  #inbreed.ped2 <- inbreed.ped2[order(inbreed.ped2[, 1], decreasing = TRUE), ]
  #inbreed.ped2 <- inbreed.ped2[, -1]
  
  
  # isolate offspring so they are only counted once (note that all offpsring made it to adult stage) 
  unique.ids   <- unique(inbreed.ped[, 1])
  m1           <- match(unique.ids, inbreed.ped[, 1]) #original, but dams must be declared before offspring (take last not first match)
  inbreed.ped2 <- inbreed.ped[m1, ]
  #rownames(inbreed.ped2) <- 1:length(inbreed.ped2[, 1])
  
  # sort pedigree using built in function
  inbreed.ped3 <- ped_sort(ped = inbreed.ped2, id = "offspring", dam = "dams", sire = "sires",  keep_names = TRUE)
  
  
  #  OUT <- NULL
  #for(n in 1:length(unique.ids)){
  # ida <- unique.ids[n]
  # m1  <- match(ida, inbreed.ped2[, 2])
  # m2  <- match(ida, inbreed.ped2[, 3])
  # m1  <- length(which(is.na(m1) == FALSE))
  # m2  <- length(which(is.na(m2) == FALSE))
  # 
    #if(m)
  # 
  # out <- cbind(n, ida, m1, m2)
  # OUT <- rbind(OUT, out)
    
  # }
  
  
  #rescue <- purgeR::ped_rename(
  #          ped = inbreed.ped2,
  #          id = "offspring",
  #          dam = "dams",
  #          sire = "sires",
  #         keep_names = TRUE
  #      )
  
  rescue <- purgeR::ped_rename(ped = inbreed.ped3, id = "id", dam = "dam", sire = "sire", keep_names = FALSE)
  
  
  rescue2 <- rescue %>% purgeR::ip_F()
  #plot(rescue2[, 1], rescue2[, 5])
  
  names2  <- rescue2[, 4]
  names2  <- unlist(strsplit(names2, "_"))
  years   <- names2[seq(from = 1, to = length(names2), by = 4)]
  individs<- which(years >= (n.years - 10)) # CALCULATING FOR THE LAST 10 YEARS!
  
  rescue3 <- rescue2[individs, ]
  mean.F  <- mean(rescue3[, 5])
  
  #unlink("../output/pedigree.txt")
  return(mean.F)
}

