Output <- function(hosts, n, i) {
   
    host.scepts <- t(data.frame(host.suscepts))  
    vars        <- cbind(n, plength, pwidth, n.species, n.loci, n.alleles, n.genotypes, n.adults, n.tads, k.adults, k.tads, n.eggs, adult.survival.var, tadpole.survival.var, proportion.metamorph, r, days, threshold, host.scepts, n.generations, n.replicates)        
   
    gtypes   <- hosts[, -c(1:8)]
    
    #begin calculating # unique genotypes (may be a useless measure to delete)
    loc.pos <- seq(from = 1, to = n.loci*2, by = 2)
    GEN <- NULL # need to sum within a locus to account for 1,2 and 2,1 genotype being equivalent
    for(l in loc.pos){
        locus <- gtypes[, c(l, l+1)]
        locus <- rowSums(locus)
        GEN <- cbind(GEN, locus)
    }
    n.gtypes <- nrow(unique(GEN))
    #gpaste   <- apply(GEN, 1, function(x){paste(x, collapse = "")}) # 
    #n.gtypes <- length(unique(gpaste))
    
    #begin calculating measures of genetic diversity
    loc.pos <- seq(from = 1, to = n.loci*2, by = 2)
    HET <- NULL
    for(l in loc.pos){
        # per locus heterozygosity
        locus <- gtypes[, c(l, l+1)]
        geno  <- length(locus[, 1])
        het   <- length(which(locus[, 1] != locus[, 2]))
        het.observed  <- het/geno

        freqs <- table(locus)
        p <- freqs[1]/(geno*2)
        het.expected <- 2*p*(1-p)

        hets <- cbind(het.observed, het.expected)
        HET <- rbind(HET, hets)
    }
    Ho <- mean(HET[, 1])
    He <- mean(HET[, 2]) #averaged across loci
    P.poly <- 1-(length(which(HET[, 1] == 0))/length(HET[, 1]))

    #table and count susceptibilities
    suscepts <- table(hosts[, 8])
    n.scepts <- names(suscepts)
    vals     <- as.vector(suscepts)
    scepts   <- paste(n.scepts, vals)
    scepts   <- paste(scepts, collapse="/")    

    out1 <- cbind(vars, n.gtypes, He, Ho, P.poly, scepts)
    #out1 <- cbind(vars, gtypes1, freqs, suscepts)

    n.individs <- length(hosts[, 1])
    n.species.t  <- length(unique(hosts[, 2]))
    tads <- hosts[hosts[, 4] == 1, ]
    n.tads.total <- length(tads[, 4])
    n.tads.inf   <- length(tads[tads[, 5] == 2, 5])
    n.tads.uninf <- length(tads[tads[, 5] == 1, 5])

    adults <- hosts[hosts[, 4] == 2, ]
    n.adults.total <- length(adults[, 4])
    n.adults.inf   <- length(adults[adults[, 5] == 2, 5])
    n.adults.uninf <- length(adults[adults[, 5] == 1, 5])

    n.inf   <- length(hosts[hosts[, 5] == 2, 5])
    n.uninf <- length(hosts[hosts[, 5] == 1, 5])

    output <- cbind(i, out1, n.individs, n.species.t, n.tads.total, n.tads.inf, n.tads.uninf, n.adults.total, n.adults.inf, n.adults.uninf, n.inf, n.uninf)
    write.table(output, paste(outdir, "Output.txt", sep = ""), col.names = FALSE, row.names = FALSE, sep="\t", append = TRUE)  

}
