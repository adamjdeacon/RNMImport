# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################

#' 
#' @param files 
#' @param noTitles 
#' @param noLabels 
#' @title Import NONMEM7 parameter iterations from .EXT files
#' @return 
#' @author fgochez

importNm7Iterations <- function( files = NULL, noTitles = NULL, noLabels = NULL, methods = NULL, path = "." )
{
	numFiles = length(files)
	if(numFiles == 0) return(list())
	
	RNMImportStopifnot(all(c(length(noTitles) == numFiles, length(noLabels) == numFiles, 
					length(methods) == numFiles)), 
			msg = "Inconsistent length of vector parameters!", match.call())
	
	RNMImportStopifnot( all(noLabels == "0") 
	, msg = "NOLABELS=1 option currently disallowed", match.call())
	
	# if an iteration file name is duplicated, it should (apparently) be disregarded.  
	files[duplicated(files)] <- ""

	iterations <- vector(mode = "list", length = numFiles)
	for(i in seq_along(files))
	{
		if(files[i] == "") next
		if(!file.exists(file.path(path, files[i]))){
			RNMImportWarning(paste(files[i], 'does not exist'))
			next		
		}
		iterations[[i]] <- importNm7Tables(file=files[i], type = "ext", tableTitles = noTitles[i] == "0" , path = path)
		names(iterations[[i]]) <- sapply(iterations[[i]], function(x) attr(x, "method"))
	}
	
	do.call(c, iterations)
}
