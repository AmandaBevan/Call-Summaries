
library(dplyr)

## path to folder that contains all of the .txt files
## NOTE: These should be full paths that probably start with "C:/Users/Amanda/.../"
## IMPORTANT: The paths need to end with a forward slash ("/") AND the outFP 
## needs to be different than the baseFP
baseFP <- "C:/Users/Amanda/Desktop/2021 Call Summaries/"
outFP <- "C:/Users/Amanda/Desktop/"

# AMANDA READ THIS: You need to have a better naming convention for the output file names
# The output shouldn't be named something based on the date you are running this script,
# but it should have something to do with the data it contains
outputFilename <- "AllFilesCombined"

## ASSUMPTIONS:
## 1) Files are named with the following convention: <transect>_<start year>_<start month abbr>_<two
##    digit start day>_<end year>_<end month abbr>_<two digit end day>
##
## 2) Values in the FileName column (filenames for .wav files) either start with characters (i.e.
##    "SMU3_") immediately followed by a date or it starts immediately with a date (format = YYYYMMDD). 
##    If a .wav filename starts with a numeric value (i.e. "123_<date>") OR there are two 
##    sets of characters before the date (i.e. "SMU3_Amanda_<date>), then the lapply below will break
##    


### ====================================================== ###
####### do NOT need to change anything below this line #######
### ====================================================== ###

fileNames <- list.files(baseFP)

allFiles <- lapply(fileNames, function(x){
  
  # read in file and make it a df
  rawCalls <- read.delim(paste0(baseFP, x), stringsAsFactors = F)
  
  ### get info from the filename
  transect <- sub("\\_.*", "", x)
  
  # get pieces of start and end dates from filename and piece together
  # NOTE: This assumes that start and end dates are in the same year
  year <- substr(gsub(paste0(transect, "_"), "", x), 1, 4)
  startMonth <- substr(gsub(paste0(transect, "_"), "", x), 6, 8)
  startDay <- substr(gsub(paste0(transect, "_"), "", x), 10, 11)
  endMonth <- substr(gsub(paste0(transect, "_"), "", x), 18, 20)
  endDay <- substr(gsub(paste0(transect, "_"), "", x), 22, 23)
  
  startDate <- as.Date(paste0(year, startMonth, startDay), format = "%Y%b%d")
  endDate <- as.Date(paste0(year, endMonth, endDay), format = "%Y%b%d")
  
  ### remove prefix characters
  # get all characters before the first underscore
  # NOTE: This assumes that all rows have the same prefix, so I just selected the first row
  prefix <- sub("\\_.*", "", rawCalls$Filename[1])
  
  ## check if the prefix is characters or numbers 
  # if the prefix is characters, then it should be removed
  # if the characters before the first underscore are numbers,
  # then it's the date...and nothing needs to be removed
  if(suppressWarnings(is.na(as.numeric(prefix)))){
    rawCalls$Filename <- gsub(paste0(prefix, "_"), "", rawCalls$Filename)
  }
  
  # create all columns we want to add
  dateColumn <- as.Date(substr(rawCalls$Filename, 1, 8), "%Y%m%d")
  yearColumn <- as.numeric(substr(rawCalls$Filename, 1, 4))
  hourColumn <- as.numeric(substr(rawCalls$Filename, 10, 11))
  
  # TODO: may need to rethink this time column. It is currently hour:minute (i.e. seconds not included)
  timeColumn <- paste0(substr(rawCalls$Filename, 10, 11), ":", 
                       substr(rawCalls$Filename, 12, 13))
  
  startDay <- dateColumn
  
  # if a call was registered before 10am, then that call's start day should be the day before
  startDay[which(hourColumn < 10)] <- startDay[which(hourColumn < 10)]-1
  
  ## ---------------------------------------------------------------------------------------------
  
  # trying to paste calls from ~Spp column (90%) into the SppAccp column (which is 99% accurate)
  # there will never be a species listed in both columns at the same time. 
  # we will miss 99% accurate calls if this isn't addressed or done by hand
  
  Spp <- if_else(rawCalls$X.Spp == "", rawCalls$SppAccp, rawCalls$X.Spp)
  Spp[which(grepl("/", Spp))] <- "no ID"
  Spp[which(Spp == "")] <- "no ID"
  
  # append all new columns with old df
  newDF <- cbind(rawCalls, dateColumn, yearColumn, hourColumn, timeColumn, startDay, Spp, 
                 Site = rep(transect, nrow(rawCalls)),
                 transectStartDate = rep(startDate, nrow(rawCalls)),
                 transectEndDate = rep(endDate, nrow(rawCalls)),
                 containingFilename = rep(x, nrow(rawCalls)))
  
  # return dataframe with new highly helpful columns
  return(newDF)
  
})# end lapply

allFilesCombined <- do.call("rbind", allFiles)

# export new csv with lots of new and highly helpful columns
write.csv(allFilesCombined, file = paste0(outFP, outputFilename, ".csv"))


### Questions for AB on Nov 9 2021:
# 1) I take the X.Spp if it's not empty. If it is empty, then I take the SppAccp. 
#    If SppAccp has a "/", then the Spp is set to ""
#    AB: Correct
#
# 2) Should Site be "T1A", "T6B", etc.? Or should Site be "T1", "T6", etc.? 
#    Do you need them separated for anything? Do you want to be able to 
#    summarize on "T1" so that "T1A", "T1MID", and "T1B" would all be included
#    in a summary?
#    AB: doesn't have an answer, so I'm going to leave Site as "T1A", "T6B", etc.
# 


