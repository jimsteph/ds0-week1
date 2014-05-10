pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # initialize variables
  datasum <- 0.0
  datacount <- 0
  
  # loop through each file and process
  for(i in id){
    # start by getting the proper file name from the id
    fname <- as(i, "character")
    flength <- nchar(fname)
    if(flength == 1) fname <- paste(directory, "/", "00", fname, ".csv", sep="")
    else
    if(flength == 2) fname <- paste(directory, "/", "0", fname, ".csv", sep="")
    else fname <- paste(directory, "/", fname, ".csv", sep="")
    
    # now that we've got a file name, let's get the file
    pollution <- read.csv(fname)
    
    # in the file, get all the valid (not NA) rows for the selected column
    # column 2 is sulfate, column 3 is nitrate
    if (pollutant == "nitrate") pcol <- pollution[!is.na(pollution[,3]),3]
    else pcol <- pollution[!is.na(pollution[,2]),2]
    
    # finally, get the count and sum of the selected column
    datacount <- datacount + NROW(pcol)
    datasum <- datasum + sum(pcol)
  }
  
  # return the mean (sum/count).  the sample output has 3 decimal places, so I'm following suit
  round(datasum/datacount, 3)
  
}