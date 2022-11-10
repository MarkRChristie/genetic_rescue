n.loci <- 2
n.alleles <- 2

n.individs = 1000

OUT <- NULL
for(n in 1:n.loci){
  zeros <- rep(0, n.individs*0.5)
  pos   <- rep(0.25, n.individs*0.75)
  neg   <- rep(-0.25, n.individs*0.75)
  out   <- c(zeros, pos, neg)
  out   <- sample(out, length(out), replace = TRUE)
  OUT <- cbind(OUT, out)
}

head(OUT)
hist(rowSums(OUT))
