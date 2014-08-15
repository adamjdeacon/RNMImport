

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
