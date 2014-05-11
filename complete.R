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
  
  # Loop through each file and process
  for (i in id) {
    
    # Read in the apropriate csv file
    comp <- read.csv(getfilename(directory, id))
    
    # Get the set of complete cases in the file
    
    # Append 'id' and 'nobs' to the results list (rbind())
    
  }
  
  # Return the results
  
}
