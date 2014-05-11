#*************************************
# Function: getfilename(directory, id)
# Input:  directory - The name of a directory relative to the current working
#      		directory.
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

#*************************************
# Function: corr(directory, threshold)
# Input:  	directory - A relative directory holding the files;
#         	threshold - A numeric vector of length 1 indicating the number of
#		  			completely observed observations (on all variables) 
#					required to compute the correlation between nitrate and
#					sulfate; the default is 0
# Output: 	A numeric vector of correlations
#
corr <- function(directory, threshold = 0) {
  # Initialize variables
  cors <- vector(mode="numeric", length=0)
  loopcount <- 0
  
  # Loop through all files
  id=1:332
  for(i in id) {
    loopcount <- loopcount + 1
    comp <- read.csv(getfilename(directory, id[loopcount]))
    
    # See if current file has at least 'threshold' complete frames
    # First, get the set of complete cases in the file
    ccases <- 0
    for(i in 1:nrow(comp)) {
      if(!is.na(comp[i,2]) & !is.na(comp[i,3])) {
        ccases<-ccases + 1
      }
    }
    # if it passes the threshold, process it
    if(ccases>threshold) {
      ccc<-cor(comp[,2], comp[,3], use="na.or.complete")
      cors <- append(cors, ccc)
    }
  }
  # Return value
  cors
}
