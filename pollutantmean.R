#*************************************
# Function: getfilename(directory, id)
# Input:  directory - The name of a directory relative to the current working
#  				directory.
#       	id - The id number of a polution monitor that corresponds to the
#					name of the file (i.e.: 1 => 001.csv, 13 => 013.csv, etc.).
# output: 	A filename of the form 'path/nnn.csv', where 'path' is the relative
#			path to the directory and 'nnn' is 'id' padded with leading zeros.
#
getfilename <- function(directory, i) {
  fname <- as(i, "character")
  flength <- nchar(fname)
  if(flength == 1) fname <- paste(directory, "/", "00", fname, ".csv", sep="")
  else
    if(flength == 2) fname <- paste(directory, "/", "0", fname, ".csv", sep="")
  else fname <- paste(directory, "/", fname, ".csv", sep="")
  fname
  
}

#**************************************************
# Function: pollutantmean(directory, pollutant, id)
# Input:  	directory - A relative directory holding the files.
#         	pollutant - "sulfate" or "nitrate", refers to colums 2 and 3 in
#					files.
#         	id - The id number of a polution monitor that corresponds to the
#					name of the file (i.e.: 1 => 001.csv, 13 => 013.csv, etc.).
#					default is all available IDs.
# Output: 	The mean of the polutant across all monitors listed in the
#         	'id' vectore (ignoring NA values.
#
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  # Initialize variables
  datasum <- 0.0
  datacount <- 0
  loopcount <- 0
  # Loop through each file and process
  for(i in id){
    
    # Read in the appropriate csv file
    loopcount <- loopcount + 1
    pollution <- read.csv(getfilename(directory, id[loopcount]))
    
    # In the file, get all the valid (not NA) rows for the selected column
    # Column 2 is sulfate, column 3 is nitrate
    if (pollutant == "nitrate") pcol <- pollution[!is.na(pollution[,3]),3]
    else pcol <- pollution[!is.na(pollution[,2]),2]
    
    # Finally, get the count and sum of the selected column
    datacount <- datacount + NROW(pcol)
    datasum <- datasum + sum(pcol)
  }
  
  # Return the mean (sum/count).  the sample output has 3 decimal places, 
  # so I'm following suit
  round(datasum/datacount, 3)
  
}
