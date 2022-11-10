Parameter_summary <- function(parameters) {
  K.adults     <- parameters[["k.adults"]]
  K.adults.var <- parameters[["adult.survival.var"]]
  n.eggs       <- parameters[["n.eggs"]]
  K.juveniles  <- parameters[["k.juveniles"]]  
  juvenile.survival.var <- parameters[["juvenile.survival.var"]]
  K.larvae     <- parameters[["pops"]][2]
  larval.survival.var <- parameters[["larval.survival.var"]]
  
  cbind(k.adults, adult.survival.var, n.eggs, K.juveniles, juvenile.survival.var, K.larvae, larval.survival.var)  
}



