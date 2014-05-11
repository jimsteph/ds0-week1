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

#**********************************
# Function: complete(directory, id)
# Input:  	directory - A relative directory holding the files;
#         	id - The id number of a polution monitor that corresponds to the
#					name of the file (i.e.: 1 => 001.csv, 13 => 013.csv, etc.);
#					default is all available IDs.
# Output: 	Returns a data frame of the form:
#				id nobs
#				1  117
#				2  1041
#                ...
#		 	where 'id' is the monitor ID number and 'nobs' is the number
#         	of complete cases
#
complete <- function(directory, id=1:332) {
  
  # Initialize the results list  
  completelist <- data.frame(1:length(id), 1:length(id))
  colnames(completelist) <- c("id", "nobs")
  
  # Loop through each file and process
  loopcount <- 0
  for (i in id) {
    
    # Read in the apropriate csv file
    loopcount <- loopcount + 1
    comp <- read.csv(getfilename(directory, id[loopcount]))
    
    # Get the set of complete cases in the file
    ccases <- 0
    for(i in 1:nrow(comp)) {
      if(!is.na(comp[i,2]) & !is.na(comp[i,3])) {
        ccases<-ccases + 1
      }  
    }
    # Append 'id' and 'nobs' to the results list (rbind())
    completelist[loopcount, 1] <- id[loopcount]
    completelist[loopcount, 2] <- ccases
  }
  
  # Return the results
  completelist
}
