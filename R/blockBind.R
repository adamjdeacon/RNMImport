



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