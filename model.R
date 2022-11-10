#==================================================================================================#
# Script created by Mark Christie, contact at markchristie1500@gmail.com
# Script created in version R 4.2.1
# This script:  Generates model output for rescue projects
# Usage notes: Set all parameters and then source this file (see markdown for details)
#==================================================================================================#
# set working directory, load packages
"C:/Program Files/R/R-4.2.1/bin/R.exe"
#setwd("C:/Users/fishf/Dropbox/manuscripts/rescue/salmon")
setwd("/mnt/c/Users/fishf/Dropbox/manuscripts/rescue/salmon")
library("hierfstat")
library("inbreedR")
base.directory <- getwd()

test <- 11

#==================================================================================================#
# first create donor populations (all functions reside in genetic_rescue)

if(length(list.files("./pops")) == 0) {
source(paste(base.directory, "./donor_population_functions/FunctionSourcer.R", sep = '')) #loads packages, sources functions, and sets source directory
data.frame(unlist(parameters)) # show all parameters
n.replicates  <- 1    # number of replicates for each combination of parameters
replicates    <- Replicates(parameters, n.replicates)

for(i in 1:length(replicates[, 1])){
 parameters[["k.adults.final"]] <- replicates[i, 1]  # final population size
 parameters[["optima"]]         <- replicates[i, 2]  # phenotypic optimum for local adapation
 model <- RunModel(n.generations, i)
}

# clear all variables and functions except for base directory
rm(list=setdiff(ls(), "base.directory"))

}
#==========================================================================================================#
# second create focal population and perform rescue (all functions reside in source)

source(paste(base.directory, "/source/FunctionSourcer.R", sep = '')) #loads packages, sources functions, and sets source directory
data.frame(unlist(parameters)) # show all parameters
n.replicates  <- 1             # number of replicates for each combination of parameters
replicates <- Replicates(parameters, n.replicates)

for(i in 1:length(replicates[, 1])){
  parameters[["k.adults.final"]] <- replicates[i, 1]  # final population size
  parameters[["optima"]]         <- replicates[i, 2]  # phenotypic optimum for local adapation
  model <- RunModel(n.generations, i)
}

