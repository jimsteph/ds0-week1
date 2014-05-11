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
  
}