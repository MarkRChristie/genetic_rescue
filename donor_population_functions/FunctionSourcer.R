#FunctionSourcer <- function() {
# Set working directory, import packages, source functions, 
setwd(paste(base.directory,"/donor_population_functions/", sep = ''))    # set temp working directory 
#library(fields)  # used for calculation of pathogen dispersal distance matrix
#source(paste(getwd(), "/Transformation_plots.R", sep = ''))
#source(paste(getwd(), "/DensityDependence_plots.R", sep = ''))
#source(paste(getwd(), "/Parameter_summary.R", sep = ''))
#source(paste(getwd(), "/Larvae.R", sep = ''))
source(paste(getwd(), "/Juveniles.R", sep = ''))
source(paste(getwd(), "/Adults.R", sep = ''))
source(paste(getwd(), "/Genotypes.R", sep = ''))
source(paste(getwd(), "/Reproduction.R", sep = ''))
#source(paste(getwd(), "/Transformation.R", sep = ''))
#source(paste(getwd(), "/LarvalMortality.R", sep = ''))
source(paste(getwd(), "/JuvenileMortality.R", sep = ''))
source(paste(getwd(), "/AdultMortality.R", sep = ''))
source(paste(getwd(), "/Output.R", sep = ''))
source(paste(getwd(), "/Parameters.R", sep = ''))
source(paste(getwd(), "/Replicates.R", sep = ''))
source(paste(getwd(), "/RunModel.R", sep = ''))
#source(paste(getwd(), "/Hatchery.R", sep = ''))
source(paste(getwd(), "/Selection.R", sep = ''))
source(paste(getwd(), "/AdditiveVariation.R", sep = ''))