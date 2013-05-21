##################################################################
# CSLtoVector
# Turns a comma-seperated list in a string into a character vector, possibly removing blanks
# and including the special string "<NONE>"
# Author: F. Gochez,
# Added: Jan 8 2009
# Last modified: Jan 9 2009
# Parameters:
# @ txt [C,1] - string to split
# @ sep [C,1] - seperator character (comma by default)
# @ removeBlank [L,1] -
# Returns: a character vector, with each element obtained by splitting txt along commas
##################################################################

CSLtoVector <- function(txt, sep =",", removeBlank = TRUE) 
{
    txt <- as.character(txt)
	sep <- if(removeBlank) sprintf("[[:space:]]*(%s)[[:space:]]*", sep ) else sep 
	outTxt <- unlist(strsplit(txt, split = sep))
	outTxt <- outTxt[outTxt != ""]
	if(any(outTxt == "<NONE>")) {
		if(length(outTxt) == 1) {
			return(character(0))
		}
		outTxt <- outTxt[outTxt != "<NONE>"]
	}
	outTxt
}
