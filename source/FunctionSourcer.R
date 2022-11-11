#FunctionSourcer <- function() {
# Set working directory, import packages, source functions, 
setwd(paste(base.directory,"/source/", sep = ''))    # set temp working directory 
source(paste(getwd(), "/Juveniles.R", sep = ''))
source(paste(getwd(), "/Adults.R", sep = ''))
source(paste(getwd(), "/Genotypes.R", sep = ''))
source(paste(getwd(), "/Reproduction.R", sep = ''))
source(paste(getwd(), "/JuvenileMortality.R", sep = ''))
source(paste(getwd(), "/AdultMortality.R", sep = ''))
source(paste(getwd(), "/Output.R", sep = ''))
source(paste(getwd(), "/Parameters.R", sep = ''))
source(paste(getwd(), "/Replicates.R", sep = ''))
#source(paste(getwd(), "/RunModel.R", sep = ''))
source(paste(getwd(), "/Selection.R", sep = ''))
source(paste(getwd(), "/AdditiveVariation.R", sep = ''))
source(paste(getwd(), "/Inbreeding_estimation_pedigree.R", sep = ''))
source(paste(getwd(), "/Rescue.R", sep = ''))
source(paste(getwd(), "/Semelparity.R", sep = ''))
