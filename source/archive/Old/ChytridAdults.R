ChytridAdults <- function(adults, host.suscepts) {
    infecteds   <- adults[adults[, 5] == 2, ]
    uninfecteds <- adults[adults[, 5] == 1, ]
    
    OUT <- NULL
    if(length(infecteds) == ncol(adults)) {infecteds <- t(data.frame(infecteds))} 
    for(s in 1:length(infecteds[, 8])){
     guys <- infecteds[s, ]
     guys <- t(data.frame(guys)) 
  	 prob <- guys[, 8]
     if(prob == 0) {prob <- 0.000001}
     if(prob == 1) {prob <- 0.999999}
     test <- sample(1:2, prob = c(prob, 1-prob))
     if(test[1] == 1) {out <- guys}
     if(test[1] == 2) {out <- NULL}
     OUT        <- rbind(OUT, out)
    }

    rownames(OUT) <- NULL
    adults <- rbind(uninfecteds, OUT)
    return(adults)
  }




