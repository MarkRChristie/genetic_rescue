#=============================================================================================================#
# Script created by Mark Christie, all rights reserved, contact at markchristie1500@gmail.com
# Script created in version R 4.2.1 on xx/xx/2022
# This script:
# Usage notes: 
#============================================================================================================#
# Set working directory, import packages, source functions, initialize global variables
setwd("C:/Users/fishf/Dropbox/manuscripts/rescue/genetic_rescue/pops")

list.files()

#source("~/Drive/scripts.R", sep = ''))

output <- read.table("out_300_0_output.txt", header=TRUE, sep="\t", na.strings="?", dec=".", strip.white=TRUE, row.names = NULL)
head(output)
output <- output[, -1]

# only write column names once if appending to file
#write.table(output.reduced, "FST_output_reduced.txt", col.names=!file.exists("FST_output_reduced.txt"), sep="\t", append = TRUE, row.names = FALSE)

#write.table(offs, "temp".txt", col.names = TRUE, sep="\t", append = FALSE)
#=============================================================================================================#

pdf(file = "300_0_donor_pops.pdf", width = 6, height = 5) 
plot(output[, 1], output[, 2], pch = 21, bg = "blue", cex = 2, main = "juveniles", ylim = c(0, max(output[, 2]) + 20)) # plot adults
plot(output[, 1], output[, 3], pch = 21, bg = "blue", cex = 2, main = "adults", ylim = c(0, max(output[, 3]) + 200)) #plot juveniles
plot(output[, 1], output[, 5], pch = 21, bg = "blue", cex = 2, main = "temperature tolerance", ylim = c(0, max(output[, 5]) + 0.5)) #plot juveniles
plot(output[, 1], output[, 6], pch = 21, bg = "blue", cex = 2, main = "Ho", ylim = c(0, max(output[, 6]) + 0.5)) #plot juveniles
plot(output[, 1], output[, 7], pch = 21, bg = "blue", cex = 2, main = "Fis") #plot juveniles
plot(output[, 1], output[, 8], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 8]) + 5)) #plot juveniles
plot(output[, 1], output[, 9], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 9]) + 5)) #plot juveniles
plot(output[, 1], output[, 10], pch = 21, bg = "blue", cex = 2, main = "Ne1", ylim = c(0, max(output[, 10]) + 5)) #plot juveniles
plot(output[-c(1,2,3), 1], output[-c(1,2,3), 11], pch = 21, bg = "blue", cex = 2, main = "Ne1") #plot juveniles

dev.off()





