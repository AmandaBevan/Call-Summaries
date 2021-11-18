
require(dplyr)

### Change the path and inputFilename below
# baseFP should be where the output file is located (the file that was generated from ProcessTxtFiles.R)
# inputFilename should be the name of the output file generated from ProcessTxtFiles.R
# NOTE: baseFP and outFP need to end with a forward slash ("/")
baseFP <- "C:/Users/Amanda/Desktop/"
inputFilename <- "AllFilesCombined"

# AMANDA READ THIS: You need to have a better naming convention for the output file names
# The output shouldn't be named something based on the date you are running this script,
# but it should have something to do with the data it contains
outFP <- "C:/Users/Amanda/Desktop/"
outputFilename <- "SummarizedCalls"

### ====================================================== ###
####### do NOT need to change anything below this line #######
### ====================================================== ###

wonderfulDF <- read.csv(paste0(baseFP, inputFilename, ".csv"), stringsAsFactors = FALSE)

## take a look at the data summarized by site, day, spp
#table(wonderfulDF$Site)
#table(wonderfulDF$startDay)
#table(wonderfulDF[which(!is.na(wonderfulDF$Spp)), "Spp"])

outputDf <- data.frame()

# Loop through species and site combinations
for(i in unique(as.character(wonderfulDF$Spp))){
  
  tmpDf <- wonderfulDF[which(wonderfulDF$Spp == i),]
  
  for(j in unique(as.character(tmpDf$Site))){
    
    siteSppDf <- tmpDf[which(tmpDf$Site == j),]
    
    averageCallsPerNight <- nrow(siteSppDf)/length(unique(as.character(siteSppDf$startDay)))
    
    outputDf[nrow(outputDf) + 1, "Spp"] <- i
    outputDf[nrow(outputDf), "Site"] <- j
    outputDf[nrow(outputDf), "AverageCallsPerNight"] <- averageCallsPerNight
    
  }
}

write.csv(outputDf, file = paste0(outFP, outputFilename, ".csv"))
