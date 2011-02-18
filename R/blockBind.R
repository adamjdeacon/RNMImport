

##################################################################
# blockBind
# Builds a matrix from a set of NONMEM-style matrix specifications
# Author: R. Francois, modifications by F. Gochez
# Added: Jan 6 2009
# Last modified: Jan 6 2009
# parameters :
# @ mList [list,+] - A list of character vectors holding NONMEM matrix specs
# @ defaultPrefix [C,1] - Default prefix for the names of the matrix rows and colunmns (e.g. OMEGAX)
# @ givenNames [N, 1] - Starting position of the title substrings returned (only used if substring is TRUE)
# Returns:  A matrix
##################################################################

blockBind <- function(
		mList, 
		defaultPrefix,
		giveNames = !missing(defaultPrefix) ) 
{
	
	### handling "SAME". See ?$OMEGA
	# 
	for( i in seq(along = mList))
	{
		if(i==1) next
		mi <- mList[[i]]
		if( is.character(mi) && length(mi) == 1 && mi == "SAME")
		{
			names <- rownames( mi ) 
			mList[[i]] <- mList[[i-1]]
			if( !is.null(names) &&length(names) == nrow(mList[[i]]) )  
				dimnames( mList[[i]] ) <- rep( list(names), 2)
		}	
	}
	mList <- lapply( mList, as.matrix )
	rowList <- sapply(mList, nrow)
	outMat <- array(0, dim = rep(sum(rowList), 2))
	rowList <- 1 + cumsum(rowList) - rowList
	
	for(i in 1:length(mList)) 
	{
		start <- rowList[i]
		end <- rowList[i] - 1 + nrow(mList[[i]])
		outMat[start:end, start:end] <- mList[[i]]
	}
	
	if( giveNames ) 
	{
		### get the names that are already there
		names <- unlist( sapply( mList, function(x){ 
							if(is.null(dimnames(x))) rep("", nrow(x)) else rownames(x)  
						}))
		### replace empty with appropriate name
		names <- ifelse( names == "", sprintf("%s%d", defaultPrefix, 1:nrow(outMat)), names  )
		dimnames( outMat ) <- rep( list(names), 2)
	}
	outMat
}